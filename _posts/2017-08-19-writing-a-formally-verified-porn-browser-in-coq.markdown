---
layout: post
author: Michael Burge
title: "Writing a Formally-Verified Porn Browser in Coq and Haskell"
started_date: 2017-08-19 05:19:00
date: 2017-08-25 18:35:00
tags:
  - coq
  - haskell
  - javascript
---

It's uncommon to use formal verification when developing software. Most people are unfamiliar with the tools and techniques, or assume they're only for specialized use.

This article will show how to write a simple image browser with:
* Core data structures and operations formally verified using the Coq theorem prover.
* A Haskell web server that handles HTTP requests
* An HTML/CSS/Javascript frontend

## Definitions

We're going to make an image browser. It's an unspoken truth that most large image databases people have are actually porn collections, so we'll be honest with ourselves and call it a porn browser.

We'll have Coq-verified data structures and operations, which some Haskell will use later on to respond to HTTP requests. Specifically, we need the ability to:

* Add new images to the database
* Get a sorted list of images by date added, or category
* Delete an image
* Add a category to an image
* Remove a category from an image
* Add categories

Here are the major types:

{% highlight coq %}
Require Import Ascii.
From Coq Require Import
     Sets.Ensembles
     NArith.BinNat
     ZArith.Int
     FSets.FMapAVL
     FSets.FSetAVL
     Structures.OrdersEx
     Structures.OrdersAlt
     Lists.List
     Strings.String.


Module N_as_OT := Backport_OT N_as_OT.
Module M := FMapAVL.Make(N_as_OT).
Module S := FSetAVL.Make(N_as_OT).

Record Image : Set :=
  mkImage
    {
      id : N;
      filename : string;
      timestamp : N
    }.

Definition ImageId := N.
Definition CategoryId := N.
Definition Index := S.t.

Record ImageDb :=
  mkDb
    {
      images  : M.t Image;
      indices : M.t Index;
      next_id : ImageId
    }.
{% endhighlight %}

`FMapAVL` and `FSetAVL` are finite maps and sets implemented using AVL trees. Our database is just a map of image ids to images, a map of category ids to sets of image ids, and an increasing id column incremented after every new addition.

It looks like the Coq developers are currently refactoring the equivalent of Haskell's `Ord` typeclass, so I use `Backport_OT N_as_OT` to generate the older interface from the newer one.

This should be the Haskell equivalent:
{% highlight haskell %}
import qualified Data.Map as M
import qualified Data.Set as S

type ImageId = Integer
type CategoryId = Integer

data Image = Image {
  imageId   :: ImageId,
  filename  :: String,
  timestamp :: Integer
}

type Index = S.Set ImageId
data ImageDb = ImageDb {
  images  :: M.Map ImageId Image
  indices :: M.Map CategoryId Index
  next_id :: ImageId
}
{% endhighlight %}

We're eventually going to generate Haskell from our formally-verified Coq implementation. What are the generated Haskell data types?
{% highlight coq %}
Extraction Language Haskell.
Extraction Image.
Extraction Index.
Extraction ImageDb.
{% endhighlight %}

Which prints
{% highlight haskell %}
-- Extraction Image.
data Image = MkImage N Prelude.String N

-- Extraction Index.
type T = Z
data Tree0 =
   Leaf0
 | Node0 T Tree0 N Tree0
type T10 = Tree0
type Index = T10

-- Extraction ImageDb.
type Key = N
data Tree elt =
   Leaf
 | Node (Tree elt) Key elt (Tree elt) T
type Bst elt = Tree elt
type T8 elt = Bst elt
data ImageDb = MkImageDb (T8 Image) (T8 Index) ImageId
{% endhighlight %}

which are essentially simple tree types hidden behind a few aliases. We also get a bunch of functions for manipulating the trees, which I left out. Our Haskell code will be using our own custom functions that manipulate these types, which will be formally-verified.

So what are these functions anyways? I'll spec out their types using the "Axiom" command:
{% highlight coq %}
Axiom newDb           : ImageDb.
Axiom create_image    : forall (db : ImageDb) (img : Image), ImageDb.
Axiom find_categories : forall (db : ImageDb) (categories : list CategoryId), list Image.
Axiom delete_image    : forall (db : ImageDb) (img : ImageId), ImageDb.
Axiom tag_image       : forall (db : ImageDb) (img : ImageId) (cat : CategoryId), ImageDb. 
Axiom untag_image     : forall (db : ImageDb) (img : ImageId) (cat : CategoryId), ImageDb.
{% endhighlight %}

This is similar to using `undefined` in Haskell for placeholders. In fact, that's pretty much what the `Extraction` command generates:
{% highlight haskell %}
create_image    :: ImageDb -> Image -> ImageDb
find_categories :: ImageDb -> (List CategoryId) -> List Image
delete_image    :: ImageDb -> ImageId -> ImageDb
tag_image       :: ImageDb -> ImageId -> CategoryId -> ImageDb
untag_image     :: ImageDb -> ImageId -> CategoryId -> ImageDb

create_image    = Prelude.error "AXIOM TO BE REALIZED"
find_categories = Prelude.error "AXIOM TO BE REALIZED"
delete_image    = Prelude.error "AXIOM TO BE REALIZED"
tag_image       = Prelude.error "AXIOM TO BE REALIZED"
untag_image     = Prelude.error "AXIOM TO BE REALIZED"
{% endhighlight %}

In Haskell, our next step would be to jump right into the implementation. In Coq, we can think for a moment about what invariants should hold:

* A new database has no images.
{% highlight coq %}
Axiom num_images :
  forall (db : ImageDb), N.

Axiom count_empty_db :
  num_images newDb = N.zero.
{% endhighlight %}

`count_empty_db` is the invariant here; `num_images` is a helper function we'll define later.

* Whenever we add an image to the database, the number of images should increase by 1:
{% highlight coq %}
Axiom size_increases :
  forall (db : ImageDb) (img : Image),
    num_images (create_image db img) = N.succ (num_images db).
{% endhighlight %}

* When we delete an image, either the image was in the database and the count of images decreases by 1, or it wasn't and the count stays the same:
{% highlight coq %}
Axiom size_decreases :
  forall (db : ImageDb) (img : ImageId),
    (Is_true (mem_image db img) /\ N.succ (num_images (delete_image db img)) = num_images db) \/
    (~ Is_true (mem_image db img) /\ num_images (delete_image db img) = num_images db).
{% endhighlight %}

* When we tag or untag an image, the number of images stays the same:
{% highlight coq %}
Axiom size_nochange :
  forall (db : ImageDb) (img : ImageId) (cat : CategoryId),
    num_images (tag_image db img cat) = num_images db /\
    num_images (untag_image db img cat) = num_images db.
{% endhighlight %}

I can think of others, but these 4 should be enough to demonstrate the technique.

## Implementation

Here are the implementations, in order:

* `newDb`
{% highlight coq %}
Definition newDb := mkDb (M.empty Image) (M.empty Index) N.zero.

(* OR *)

Definition newDb := {|
  images  := M.empty Image;
  indices := M.empty Index;
  next_id := N.zero
  |}
{% endhighlight %}

* `create_image`
{% highlight coq %}
Definition create_image (db : ImageDb) (img : Image) :=
  let newImg := {|
        id := next_id db;
        filename := filename img;
        timestamp := timestamp img
      |}
  in {|
    images := M.add (next_id db) newImg (images db);
    indices := indices db;
    next_id := N.succ (next_id db)
  |}.
{% endhighlight %}

You can test it with the `Compute` command:
{% highlight c %}
Compute create_image newDb (mkImage 0 "testing" 0).
{% endhighlight %}

which prints:
{% raw %}
     = {|
       images := {|
                 M.this := M.Raw.Node (M.Raw.Leaf Image) 0%N
                             {| id := 0; filename := "testing"; timestamp := 0 |}
                             (M.Raw.Leaf Image) 1%Z;
                 M.is_bst := M.Raw.Proofs.add_bst 0%N
                               {| id := 0; filename := "testing"; timestamp := 0 |}
                               (M.Raw.Proofs.empty_bst Image) |};
       indices := {|
                  M.this := M.Raw.Leaf Index;
                  M.is_bst := M.Raw.Proofs.empty_bst S.MSet.t_ |};
       next_id := 1%N |}
     : ImageDb
{% endraw %}
It looks scarier than it is: It's showing us the guts of the binary search trees it makes, while Haskell uses `fromList` in its `Show` instance to hide that:

{% highlight haskell %}
create_image newdb (mkImage 0 "testing" 0)
-- PRINTS
MkImage {
  images = fromList [(0, Image 0 "testing" 0)],
  indices = fromList [],
  next_id = 1
  }
{% endhighlight %}

* `find_categories`
{% highlight coq %}
Fixpoint set_from_list (xs : list N) : Index := fold_right S.add S.empty xs.
Fixpoint list_from_set (xs : Index) : list N := S.fold cons xs nil.

Fixpoint find_categories_ids (db : ImageDb) (categories : list CategoryId) : Index :=
  match categories with
  | nil => set_from_list (map fst (M.elements (images db)))
  | cons cat cats => S.inter (find_category_ids db cat) (find_categories_ids db cats)
  end.

Fixpoint find_imgs (db : ImageDb) (imgs : list ImageId) :=
  match imgs with
  | nil => nil
  | cons i is =>
    match M.find i (images db) with
    | None => find_imgs db is
    | Some img => cons img (find_imgs db is)
    end
  end.  

Fixpoint find_categories (db : ImageDb) (categories : list CategoryId) : list Image :=
  find_imgs db (list_from_set (find_categories_ids db categories)).
{% endhighlight %}

In Haskell, let bindings and top-level definitions are recursive by default. In Coq, we explicitly mark functions as `Fixpoint` to allow recursion.

* `delete_image`
{% highlight coq %}
Fixpoint delete_image (db : ImageDb) (img : ImageId) : ImageDb :=
  {|
    images  := M.remove img (images db);
    indices := M.map (S.remove img) (indices db);
    next_id := next_id db
  |}.
{% endhighlight %}

* `tag_image`
{% highlight coq %}
Fixpoint tag_image (db: ImageDb) (img : ImageId) (cat : CategoryId) : ImageDb :=
  let idxs :=
      match M.find cat (indices db) with
      | None => M.add cat (S.singleton img) (indices db)
      | Some idx => M.add cat (S.add img idx) (indices db)
      end
  in {|
    images  := images db;
    indices := idxs;
    next_id := next_id db
  |}.
{% endhighlight %}

* `untag_image`
{% highlight coq %}
Fixpoint untag_image (db : ImageDb) (img : ImageId) (cat : CategoryId) : ImageDb :=
  let idxs :=
      match M.find cat (indices db) with
      | None => indices db
      | Some idx => M.add cat (S.remove img idx) (indices db)
      end
  in {|
    images  := images db;
    indices := idxs;
    next_id := next_id db
  |}
{% endhighlight %}

## Theorems

We're almost ready to prove some theorems. Let's implement those helper functions we deferred:
{% highlight coq %}
Fixpoint num_images (db : ImageDb) : nat := M.cardinal (images db).
Fixpoint mem_image (db : ImageDb) (img : ImageId) : bool := M.mem img (images db).
{% endhighlight %}

The first theorem we can do just by calculation:
{% highlight coq %}
Theorem count_empty_db :
  num_images newDb = 0.
{% endhighlight %}

A lot of these proofs consist of a sequence of imperative mutations to a "proof context". These mutations are called "tactics". If you use a tool like [Proof General](https://proofgeneral.github.io/), you can step through each tactic to see how it changes the proof context. I'll add annotations above each tactic, showing the proof context.
{% highlight coq %}
Proof.
(* 1 subgoal, subgoal 1 (ID 46)
  
   ============================
   num_images newDb = 0
*)
  compute.
(* 1 subgoal, subgoal 1 (ID 47)
  
   ============================
   0 = 0
*)
  tauto.
(* No more subgoals. *)
Qed.
{% endhighlight %}

We start off with the statement we're trying to prove as our "goal". A theorem is proved when there are no remaining goals. We reduce the lefthand side using `compute`. The goal becomes `0 = 0`, which is a tautology that the `taut` tactic can complete.

Not too difficult, right? The follow-up ones were involved enough that I'll have to leave their detailed proofs on [Github](https://github.com/MichaelBurge/pornview).

Now, you might be curious: "Coq lets us extract code to Haskell. What exactly does a theorem convert to?" Here's your answer:

{% highlight haskell %}
-- Extraction count_empty_db.
count_empty_db :: ()
count_empty_db = __
{% endhighlight %}

You can't do it - Haskell's type system isn't powerful enough to handle theorem proving. Roughly, things that can be extracted into living, breathing computer code lie in Coq's `Set`; while the world of theorems and proofs is Coq's `Prop`. And the classic tripup for beginners is "bool vs. Prop"

A `bool` is decidably `true` or `false`. A `bool` when extracted is eventually going to live on the system heap. A `bool` has a bitstring representation.

A `Prop` is a statement in an intuitionist logic. In classical logic, you know for any proposition `P`, that `P | ~P`. You can actually add that axiom to Coq, but it's fundamentally not constructive: Just because you've excluded the possibility of `~P` doesn't mean that you can physically instantiate a bitstring representing a specific `P` in memory somewhere.

Inductive types like `bool` are __closed__. There are only 2 constructors `true` and `false`, and if you've excluded `true` you know it's `false`.

Propositions are __open__ world: You can't conclude a positive from a bunch of negatives.

Let's try the next one:
{% highlight coq %}
Theorem size_increases :
  forall (db : ImageDb) (img : Image),
    num_images (create_image db img) = num_images db + 1.
{% endhighlight %}

This theorem is actually false! All of **our** functions should be safe, but anyone can call `mkImageDb` with a `next_id` that overlaps an existing image. Then, calling `create_image` would overwrite it, and `num_images` wouldn't change.

Se we need to specify that this theorem only holds for `InternallyConsistent` databases, and that all of our operations preserve this property.

The only consistency check we'll have for now is that `next_id` should not exist in the `images` map:
{% highlight coq %}
Definition InternallyConsistent (db : ImageDb) :=
  forall (some_id : ImageId),
    M.In some_id (images db) -> N.lt some_id (next_id db).
{% endhighlight %}

Now we'll prove that `newDb` is internally consistent, using [empty_in_iff](https://coq.inria.fr/library/Coq.FSets.FMapFacts.html).
{% highlight coq %}
Theorem preserves_consistency_1 :
  InternallyConsistent newDb.
Proof.
  intro.
  intro.
  rewrite MF.empty_in_iff in H.
  contradiction.
Qed.
{% endhighlight %}

That was easy because `InternallyConsistent` is a property of the indices in the `images` map. In `newDb`, the map is empty so the statement is vacuously true.

I'll pick one more interesting one before moving on. 
{% highlight coq %}
Require Import Coq.FSets.FMapFacts
(* Earlier: Module M := FMapAVL.Make(N_as_OT). *)
Module MF := Facts M.

Theorem preserves_consistency_2 :
  forall (db : ImageDb) (img : Image),
    InternallyConsistent db -> InternallyConsistent (create_image db img).
Proof.
  intros.
  intro.
  intro.
  simpl.
  apply N.lt_succ_r.
  simpl in H0.
  apply MF.add_in_iff in H0.
  destruct H0.
  apply N.le_lteq.
  right.
  symmetry. apply H0.
  apply H in H0.
  apply N.le_lteq.
  left.
  apply H0.
Qed.
{% endhighlight %}

Recall that `images` is a finite map. We import all of the propositions relating to finite maps using the `Facts` functor, applied to the FMapAVL implementation we instantiated earlier.

Here's the idea of the proof in words:

`MF.add_in_iff` says that "If you add an element to a map, any element in the new map was either already in the map or had the same key as the one we added.". If it was already in the map, then the `InternallyConsistent` hypothesis already applies to it. Otherwise, the key we added was exactly `next_id`, and the definition of `InternallyConsistent` gives `next_id db < succ (next_id db)` after we increment it in `create_image`.

Full proofs for the theorems are in the [Github](https://github.com/MichaelBurge/pornview) repository.

## Haskell Server

Coq allows us to map inductive types directly to Haskell. There are a few built-in modules for strings, and I'll include examples for `list` and `option` also:

{% raw %}
From Coq Require Import
     extraction.ExtrHaskellString
     extraction.ExtrHaskellNatInteger
     extraction.ExtrHaskellNatNum

Extract Inductive list    => "[]" ["[]" "(:)"].
Extract Inductive option => "Prelude.Maybe" ["Prelude.Just" "Prelude.Nothing"].
Extraction Language Haskell.
Extraction "Database" Database.
{% endraw %}

Once we set the extraction options and run the `Extraction` command, we'll end up with a `Database.hs` file that can be imported by our Haskell code.

The routing part of the web server is here:

{% highlight haskell %}
import Network.Wai as W
import Network.Wai.Handler.Warp

pv_port = 1234
db_file = "images.db"
collection_dir = "images"

data ServerState = ServerState {
  state_db         :: TVar DB.ImageDb,
  state_categories :: TVar (M.Map String DB.N)
  }

type PageT a = (
     ?state :: ServerState,
     ?req :: Request,
     ?respond :: Response -> IO ResponseReceived)
     => a -> IO ResponseReceived
type Page = PageT ()

main :: IO ()
main = do
  state <- initialize
  run pv_port $ \req respond ->
      let ?state = state
          ?req = req
          ?respond = respond
      in do
        putStrLn (show req)
        case (pathInfo req, requestMethod req) of
          ([], "GET") -> pageIndex ()
          ("static" : xs, "GET") -> pageStatic $ T.unpack $ T.intercalate "/" xs
          ("images" : imageId : _, "GET") -> pageImage $ param_n imageId
          ("api" : "listCategories" : _, "GET") -> pageListCategories ()
          ("api" : "listImages" : _, "GET") -> pageListImages ()
          ("api" : "tagImage" : imageId : catName : _, "POST") -> pageTagImage (param_n imageId, catName)
          ("api" : "untagImage" : imageId : catName : _, "POST") -> pageUntagImage (param_n imageId, catName)
          _ -> respond (W.responseLBS status404 [] "???")
{% endhighlight %}

And here's a fragment of Javascript for interacting with this API:
{% highlight javascript %}
    let mkFuncs = function(funcs) {
        return {
            fetchJson: function(uri, options, onFetch) {
                window.fetch(uri, options).then(function(response) {
                    if (! response.ok) {
                        console.log("Error: ", response);
                        return;
                    }
                    response.json().then(onFetch);
                });
            },
            tagImage: function(imageId, cat, onTag) {
                funcs.fetchJson("/api/tagImage/" + imageId + "/" + cat, {method: "POST"}, onTag);
            },
            untagImage: function(imageId, cat, onUntag) {
                funcs.fetchJson("/api/untagImage/" + imageId + "/" + cat, {method: "POST"}, onUntag);
            }
        }
    };
{% endhighlight %}

In Haskell, a `let` binding creates a group of mutually-recursive functions. We can do that in Javascript with a Y combinator, or we can unroll it 4-5 times(whatever the maximum stack depth we want):
{% highlight javascript %}
    let funcs;
    funcs = mkFuncs(funcs);
    funcs = mkFuncs(funcs);
    funcs = mkFuncs(funcs);
    funcs = mkFuncs(funcs);
{% endhighlight %}

## Conclusion

Hopefully this example shows that there's nothing really stopping anyone from using Coq in their Haskell programs today. You can see the full repository on [Github](https://github.com/MichaelBurge/pornview).

Future Coq-related articles may cover:
* An overview of the common tactics and techniques used in proofs
* Defining custom LTAC commands to automate proofs
* Writing a more robust Coq-Haskell compatibility layer.
* Proving mathematical theorems using Coq
* Implementing compilers or cryptographic primitives in Coq
* Using GHCJS on the resulting Haskell to get formally-verified Javascript

Future formal verification articles may cover:
* Using Liquid Haskell to add lightweight proofs to your existing Haskell code
* Using Idris to develop e.g. a bitcoin exchange
* Using randomized testing as a cheap substitute for a formal proof.


Let me know if you have a particular preference you'd like to see next.
