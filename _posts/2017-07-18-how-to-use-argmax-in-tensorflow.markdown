---
layout: post
author: Michael Burge
title: "How do you use argmax and gather in Tensorflow?"
date: 2017-07-18 20:52:00 -0700
tags:
  - tensorflow
---

An array $$a$$ has at least one maximum element $$x$$. $$argmax$$ in its simplest case gives the index $$i$$ of this maximum element, so that $$a_i = max(a)$$. The Tensorflow argmax requires you to designate one dimension of your tensor to act as that array.

{% highlight python %}
import tensorflow as tf

sess = tf.InteractiveSession()

# Since x[3] is the largest number(4), argmax(x) is 3
x = [ 1,2,3,4,3,2,1 ]
tf.argmax(x, axis=0).eval()
# 3

# Axis 1 will apply argmax to the rows
# Axis 0 will apply argmax to the columns
x = [[ 1,2,3,4,3,2,1],
     [ 2,3,4,3,2,1,0]]

tf.argmax(x, axis=0).eval()
# [3,2]
tf.argmax(x, axis=1).eval
# [1,1,1,0,0,0,0]

{% endhighlight %}

Once you have some indices, you can convert them to values using gather. gather in its simplest case is equivalent to doing an array lookup: $$gather(a, i) = a[i]$$.

{% highlight python %}
import tensorflow as tf

sess = tf.InteractiveSession

# A scalar index of a vector is a scalar.
# 3 is the index of 4, so gathering 3 is 4.
x = [ 1,2,3,4,3,2,1 ]
tf.gather(x, 3).eval()
# 4

# A scalar index of a matrix x is a vector.
# Row 0 of x is [1,2,3,4,3,2,1], so gathering 0 returns it.
x = [[ 1,2,3,4,3,2,1],
     [ 2,3,4,3,2,1,0]]
tf.gather(x, 0).eval()
# [1,2,3,4, 3, 2, 1]

# Gathering a vector index produces a scalar index for each element of the vector, and then concatenates them.
# Gathering [1,0] produces row 1 and row 0. Concatenating them flips the rows in the original matrix.
tf.gather(x, [1,0]).eval()
# [[ 2,3,4,3,2,1,0],
   [ 1,2,3,4,3,2,1]]

# In general, each scalar index within the index tensor will produce an entire row in x.
tf.gather(x, [[[1]],[[0]]]).eval()
# [[[2,3,4,3,2,1,0]],
   [[1,2,3,4,3,2,1]]]
{% endhighlight %}

gather assumes that each scalar is an index along the first dimension. You can use gather_nd to target other values:

{% highlight python %}
import tensorflow as tf
sess = tf.InteractiveSession()

x = [[ 1,2,3,4,3,2,1],
     [ 2,3,4,3,2,1,0]]

# An empty index returns the entire tensor.
i = tf.cast([], tf.int32)
tf.gather_nd(x, i).eval()
# [[ 1,2,3,4,3,2,1],
#  [ 2,3,4,3,2,1,0]]

# The row 0 of x is [1,2,3,4,3,2,1], so gathering [0] returns it.
tf.gather_nd(x, [0]).eval()
# [1,2,3,4,3,2,1]

# Row 0, column 0 of x is 1, so gathering [0,0] returns 1.
tf.gather_nd(x, [0,0]).eval()
# 1

# The last dimension(or the most deeply-nested list) of the index tensor is an index vector using the previous rules.
# So if we stack two copies of the previous index tensors, we'll get two copies of the result:
# Two empty indices return two copies of the entire tensor
i = tf.cast([[],[]], tf.int32)
tf.gather_nd(x, i).eval()
# [[[ 1,2,3,4,3,2,1],
#   [ 2,3,4,3,2,1,0]],
#  [[ 1,2,3,4,3,2,1],
#   [ 2,3,4,3,2,1,0]]]

# Gathering [0] twice returns the first row twice.
tf.gather_nd(x, [[0],[0]]).eval()
# [[1,2,3,4,3,2,1],
   [1,2,3,4,3,2,1]]

# Gathering [0,0] twice returns row 0, col 0 twice:
tf.gather_nd(x, [[0,0],[0,0]]).eval()
# [1,1]
{% endhighlight %}

You should know the rules for all three functions now. As a homework assignment, open a python session and fill in the below puzzle to make the assertion pass:

Handy Tools for working with indices:
* [range](https://www.tensorflow.org/api_docs/python/tf/range)
* [meshgrid](https://www.tensorflow.org/api_docs/python/tf/meshgrid)
* [stack](https://www.tensorflow.org/api_docs/python/tf/stack)

{% highlight python %}
import tensorflow as tf
sess = tf.InteractiveSession()

# x is a random rgb color image.
x = tf.random_uniform([32, 32, 3 ], maxval=255, dtype=tf.int32).eval()

# Use argmax to calculate which color has the largest value. Shape = [32, 32]
indices = ??? 

# Use meshgrid and stack to generate coordinates for dimensions except the color, and then add our color indices to the end. Shape = [32, 32, 3]
coords = ???

# Select only that largest color using gather_nd to "greyscale" the image. Shape = [32, 32]
y = ???

# Write both images to disk so you can compare.
tf.write_file("original.png", tf.image.encode_png(x)).run()
tf.write_file("greyscale.png", tf.image.encode_png(y)).run()

{% endhighlight %}

If you get stuck, see the solution below:

{:.spoiler}
{% highlight python %}
import tensorflow as tf
sess = tf.InteractiveSession()

# x is a random rgb color image.
x = tf.random_uniform([32, 32, 3 ], maxval=255, dtype=tf.int32).eval()

# Use argmax to calculate which color has the largest value
indices = tf.argmax(x, axis=2).eval()

# Use meshgrid and stack to generate coordinates for dimensions except the color, and then add our color indices to the end.
coords = tf.stack(tf.meshgrid(tf.range(0,32), tf.range(0,32)) + [ indices ], axis=2).eval()

# Select only that largest color using gather_nd to "greyscale" the image
y = tf.gather_nd(x, coords).eval()

# Write both images to disk so you can compare.
tf.write_file("original.png", tf.image.encode_png(x)).run()
grey = tf.cast(tf.reshape(y, [32, 32, 1]), tf.uint8)
tf.write_file("greyscale.png", tf.image.encode_png(grey)).run()
{% endhighlight %}