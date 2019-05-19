---
layout: post
author: Tian Qi and Michael Burge
title: "Beating Mario with Reinforcement Learning"
date: 2019-05-12 23:00
tags:
  - machine_learning
  - python
  - rust
js_files:
  - https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML

---

Super Mario Bros is a video game originally released for the Nintendo Entertainment System(NES) in 1985. We developed an AI agent that passes some levels in this game - replicating a paper from members of [OpenAI and UC Berkeley](https://arxiv.org/abs/1808.04355) with similar ideas. You can see OpenAI's blog [here] (https://openai.com/blog/reinforcement-learning-with-prediction-based-rewards/)

The central idea is to use **Curiosity** as a reward. The agent
* Attempts to predict future states
* Prefers to enter states that it's unable to predict well.

In Mario, this generally means moving right or finding bonus rooms. With the exception of a few "puzzle" levels[^1], these are enough to complete many levels.

<!-- -->
Table of Contents
* This list is replaced with the Table of Contents during page generation
{:toc}

## Results

We recorded a video with an interesting subset of levels below:

<iframe width="560" height="315" src="https://www.youtube.com/embed/PiHsOFmj8ts" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Each level was trained independently with a maximum of 150,000 updates.

| Level | Pass? | Description |
| ---   | ---   | --- |
| 1-1   | <span style="green">Yes</span> | This was the most-tested level during development. It consistently beats it, but the time required is a [**Bimodal Distribution**](https://en.wikipedia.org/wiki/Mcan make it to the three small platforms before dyinuickly if it finds a hidden room that skips most of the level. |
| 1-2 | <span style="green">Yes</span> | There is a narrow section that requires Mario to wait until enemies pass through, contrary to the general policy of “moving right”. The agent consistently passes this level, usually without finding the [Warp Zone](https://www.mariowiki.com/Warp_Zone) until much later on. |
| 1-3 | <span style="red">No</span> | The end of the level uses slightly different tiles, which confuse the feature detector. That part trains slowly, since it has to beat most of the level to get there. The agent should be able to beat this level with more data and time. |
| 1-4 | <span style="green">Yes</span> | The first bowser castle, introducing spinning traps and the "fake Bowser".|
| 2-2 | <span style="red">Yes</span> | The first water level uses a different movement mechanic and homing enemies ([Blooper](https://www.mariowiki.com/blooper)). The agent can pass the level, however it is very rare. It struggles against choke points guarded by Bloopers. |
| 2-3 | <span style="red">No</span> | Mario must cross a long bridge while being constantly assaulted by flying fish ([Cheep Cheeps](https://www.mariowiki.com/Cheep_Cheep)). The agent can make it to the three small platforms before dying. The agent should be able to beat this level with more data and time. |
| 2-4 | <span style="red">Yes</span> | The second bowser castle, with more danger and an obstacle that makes it harder to jump over Bowser. The agent can beat this level, but it is rare. It usually bumps into Bowser. |
| 3-4 | <span style="red">Yes</span> | The third bowser castle, with a more restrictive blockade at Bowser and more dangerous traps. Again, while Mario can beat this level, it is rare as he dies often to Bowser. |
| 4-1 | | Introduction of Lakitu, who follows Mario and throws Spiny Eggs to deter him. The agent can consistently beat this level. |
| 8-3 | <span style="red">No</span> | The second last level, which contains an assortment of enemies. Koopas, Bullet Bills, Hammer Bros and Pirana Plants. I suspect this level could be beaten if I used more parallel environments and bigger batch sizes. |
| 8-4 | <span style="red">No</span> | The final level, which contains a water section and a non-linear map. There is a specific route Mario must follow, or else it gets sent back in the level. Mario is able to get past the first loop, but he never gets far after that. |

You can download code used to train the agent from [TODO: Create a Github repository and add the link here]().

### Behavior

There are a few interesting behaviors common to all levels:

* The agent alternates between running and sprinting (Holding B). The variation in speed makes future states harder to predict over than maintaining a constant speed.

* Sometimes the network degenerates into a bad policy, staying still or getting stuck at a wall. These generally resolve themselves

* Once the agent completes a level, it does not consistently complete the level. There is no explicit reward for doing so, and it may temporarily find higher surprisals in paths that do not complete the level.

* We expected a "wall" of surprisals for pixels at the right edge of the screen, as new parts of the level that scroll into view are unpredictable. However, the Inverse Dynamics network doesn't use the edge of the screen to predict actions taken, so the Forward Dynamics network cannot operate on those pixels.

## Concepts

A curious agent should explore and understand its environment. "Understand" means predicting its future environment. "Exploring" means preferring actions that lead to states with incorrect or unconfident future predictions. Following the original paper, we call this unexpectedness the **Surprisal**.

The environment has an associated **State**. The state is the set of inputs the network can use to make a decision. In general, this could include the pixels in a frame, recently generated audio, or text mapped with an embedding. We'll define the exact state we use later.

Each state has a set of associated **Moves**. A move is an output that causes the environment to transition to a different state. There are 14 moves the agent can choose from. This subset prevents it from pausing or pressing left/right at the same time. They are:

* NOTHING
* UP, DOWN, LEFT, RIGHT
* A, B, A + B
* RIGHT + {A, B, A + B}
* LEFT + {A, B, A + B}

Given a state, the agent should choose the **Move** that provides the biggest **Reward**. Reward is not the same as surprisal. The surprisal is the unexpectedness of a single state, but the agent should factor in future surprisals as well.

To play a level of Mario, the trained agent repeatedly:
* Retrieves the current state from the NES emulator[^2]
* Sends the state to a neural network to get a recommended action
* Sends the action to the emulator

### State

The state is not a single frame. In Mario, it's important to hold the jump button to make a higher jump. So when the agent sends an action, it is held for 15 frames. We collect the 5th, 10th and 15th frame; resize, crop, and grayscale them; and then stack them to create the state.

The resized frames have a width and height of 84 and 110. The top 18 and bottom 8 pixels are cropped: The top 18 pixels only contain the score, lives, and coin counter; and the bottom 8 pixels contain nothing of value. The NES has a low number of colors, so grayscaling doesn't drop important information and reduces the size of the input. After stacking 3 frames, our state becomes 84 x 84 x 3.

We choose to use stacked frames rather than single frames for these reasons:
* The Mario sprite frequently changes while walking. Our network is not recurrent, so it has no memory of the past. So it has no way to determine if the sprite will change in the next frame, and will always be surprised. The 5-frame gap causes the sprite to change every sample, so it is not surprising.
* The sampling gap increases the feedback related to our choice of action. The difference between moving right for 1 frame and moving right for 15 frames is much easier to see. So the agent more quickly prefers moving right.
* There are many pipes and gaps that require a full jump to pass. The agent must hold jump for at least 30 frames. It is unlikely that our network randomly stumbles upon this action sequence. With our definition of states, the agent only has to hold jump for 2-4 frames. (This is is a well known exploration problem. See: [TODO: Add a link to something explaining the Ledge Problem]()).
* 3 frames allows our agent to approximate Mario’s velocity and acceleration. The velocity distinguishes between rising and falling sprites. The acceleration might be helpful determining whether it can jump over obstacles.
* The OpenAI paper stacked 4 frames with 3 frame gaps. We use a 3-stack to help visualize the state: Each greyscale frame can be assigned to the Red, Green, or Blue color channel. It didn't seem to decrease network performance.

After the state is created, it is **Normalized** by subtracting the **mean state** and dividing by the **mean std**([**Standard Deviation**](https://en.wikipedia.org/wiki/Standard_deviation)). This gives better results than another common way of normalizing pixels: Subtracting by 127.5 and dividing by 255.

The mean state and mean std are calculated from the collected states from making 10,000 random moves at the beginning of the level. Each pixel in the state has its own mean, but the standard deviation is averaged to produce a single number. Here are a few images showing what mean states looks like [TODO]().

### Surprisal vs Reward

An action changes a state. All differences between the old and new state can be attributed to:

* Level change and black screens from death/level transition
* Camera scrolling
* Sprites movings or changing

and this is ordered from largest to smallest expected change. Larger changes are harder to predict, so a level change should be more interesting than a slightly displaced Goomba.

Dying gives a large surprisal, comparable to entering a new level: There is a very large surprisal when the screen goes black, followed by another large surprisal when the level restarts. So using surprisal as a reward would incentivize the agent to choose death when it is otherwise avoidable.

This is undesirable, but can be avoided using a different reward.

The reward for an initial state $$\text{state}(0)$$ is defined inductively as:
* The environment is measured as $$\text{state}(0)$$
* The model produces an action $$\text{action}(0)$$ and an expected state $$\text{guess}(1)$$
* $$\text{action}(0)$$ is sent to the environment to advance it to $$\text{state}(1)$$
* The difference between $$\text{state}(1)$$ and $$\text{guess}(1)$$ is $$\text{surprisal}(0)$$

This can be repeated with $$\text{state}(1)$$ to produce $$\text{surprisal}(1)$$. If we let the agent iterate like this infinite times, we will get an infinite stream of surprisals.

The ideal reward is the infinite sum:

$$
\begin{split}
\text{reward}_{\text{ideal}}(i) & = \text{discount}^0 * \text{surprisal}(i) + \text{discount}^1 * \text{surprisal}(i+1) + ... \\

&= \sum_{k=0}^\infty \text{discount}^k * \text{surprisal}(i+k)
\end{split}
$$

where the discount is a number between 0 and 1.[^4] Since $$\text{discount}^n$$ converges to 0 as $$n$$ increases, distant surprisals have less impact than closer ones. We can't keep infinite states and guesses in memory, so the actual reward is slightly different - see the [Reward](#reward) section for details.

With this reward, the network isn't as incentivized to die. It still gets a large surprisal on death, but since it reenters the same level all other surprisals will be close to zero. So the strategy of "moving right" is preferred over dying, and in Mario that is usually enough to beat the level.

## Model

The model is two distinct networks: **Policy** and **Dynamics**. Dynamics can be further broken down into **Inverse Dynamics** and **Forward Dynamics**.

These are their responsibilities:

Policy (PPO)
* Estimate the expected reward for a given state
* Provide a **Preference Score**(score) for the 14 actions at a given state.
* Choose one of the actions. This is usually the action with the highest score, but there is some randomnesss.
* During training, prevents the **Trajectory** from changing too fast. The trajectory is the sequence of states produced by following this policy.

Inverse Dynamics
* A [**Convolutional Neural Network**(CNN)](https://en.wikipedia.org/wiki/Convolutional_neural_network) maps states to **Feature Vectors**. A feature vector is 512 floating point numbers, each representing an abstract pixel texture that the network deems useful. This CNN is named the **Feature Mapper**.
* The inputs to this network are two consecutive states. Both states are mapped to feature vectors, and the output is a prediction of which action was taken.
* Since we are providing two consecutive states, we already know which action was taken. The true purpose of this network is to train the Feature Mapper.

Forward Dynamics
* The inputs to this network are a state and an action. The output is the predicted feature vector of the next state.
* The surprisal is the L2 difference between the feature vector predicted here and the actual feature vector.
* The Feature Mapper is prevented from training. This prevents the network from learning a degenerate mapping like "the feature vector is always zero", which is very easy to predict. This training of the Feature Mapper is the main reason for the Inverse/Forward split.
* The Feature Mapper is an encoder for states. The network does not try to predict the next state, but the next state as encoded by the Feature Mapper. The network has flexibility to decide what is and isn’t important in a state.

The Policy and Dynamics networks act as an [Actor-Critic Pair](TODO). The Critic tells the Actor how good or bad the move it made was, which the Actor uses to adjust action preferences.

### Training and Rollout

The agent alternates between playing and training. It makes 128 moves to collect data, which is then used to train the network. These continuous segments of play are **Episodes**.

A **Trajectory** is the collection of `(state, action)` pairs gathered during a single episode.

A **Rollout** is the trajectory along with many metrics calculated from the collected information, such as the `surprisal(i)` values.

Since the surprisal is defined using the next state we actually need 129 states as input. The very last state is called the **tail state** and is not trained on.

Because the policy includes a small amount of randomness, we can run multiple Mario environments in parallel to collect diverse data. All environments use the same neural network to infer action and metrics. During training, we repeatedly sample one or more rollouts as mini-batches. We used 8 parallel environments, one rollout per mini-batch, and we train on all 8 mini-batch 3 times for a total of 24 mini-batches per episode. Training only uses the rollout from the current episode, so we don’t need to keep the rollouts for future updates.

If the batches only contains a small subsection of the level, then the network update can cause the policy to become ill-fit to pass the parts of the level that were not seen in the batch. As a result, performance increases with bigger batches, which requires more data. The original paper uses 128 to 2024 parallel environments and noticably outperforms our experiments.


### Dynamics

![Dynamics Network](/assets/articles/20190512-marai/dynamics-network.svg)

This network contains a CNN called the Feature Mapper that converts states into feature vectors.

Inverse Dynamics concatenates each state's feature vector with its subsequent feature vector, feeds that through two Dense layers, and using a [Softmax Cross Entropy](https://www.tensorflow.org/api_docs/python/tf/nn/softmax_cross_entropy_with_logits) to decide which action to take. The actual action taken is used as input to calculate the loss. As mentioned earlier, the purpose of Inverse Dynamics is to train the Feature Mapper - the CNN at the start of the network.

Forward Dynamics attempts to predict the feature vector of the next state given the current state and an action. It puts the feature vector through multiple [Residual Layers](https://en.wikipedia.org/wiki/Residual_neural_network). The action is represented as a [One-hot Vector](https://en.wikipedia.org/wiki/One-hot). The final Dense layer has 512 features, representing the predicted features of the next state. The loss of Forward Dynamics is the L2 difference between the predicted and actual feature vectors of the next state. This loss is known as the surprisal.

The `Dense Resnet Layer` node was implemented in two different ways: OpenAI's original code used the Bottom-Left component in the Dynamic Graph, while we used the Bottom-Right component. As a result, our "Dense Residual Network" is not actually residual.

### Reward

The rollout contains 129 states. The ideal reward for `state(i)` is

$$
reward_{ideal}(i) = \sum_{k=0}^\infty discount^k * surprisal(i+k)
$$

We use Forward Dynamics to get 128 surprisals from the 129 states and semi-normalize them by dividing against a running standard deviation. This reduce variance and stabilizes training. We keep one running std per environment.

The definition of $$reward_{ideal}$$ requires infinitely many surprisals, but there are a limited amount available - especially for the last states. We work around this by bootstrapping using an estimated reward: The Policy Network estimates the true reward, and we combine that with the 128 surprisals to calculate a target reward. So the reward is a somewhat circular definition:

$$
\text{target_reward}(i) = \text{surprisal}(i) + \text{discount} * \text{predicted_reward}(i+1)
$$


We define
$$
\text{value_loss}(i) = 0.5 * (\text{target_reward}(i) - \text{predicted_reward}(i))^2
$$

The CNN is shared between three loss functions($$\text{value_loss}$$, $$\text{entropy_loss}$$, and $$\text{pg_loss}$$), which each attempt to update its weights. The factor of `0.5` reduces the importance of this loss relative to the others. `entropy_loss` has a weight of `0.001` while `pg_loss` has a weight of `1.0`.

As the network trains, `estimated_reward` and `target_reward` should converge to each other. A corollary is that `estimated_reward(i) - discount * estimated_reward(i+1)` should converge to `surprisal(i)`.

The Advantage Function `A(state, action)` - defined below - gives the benefit of the action relative to other actions available at that state. It is positive if the action is better than average, and negative if worse.

It is defined using rollup similar to `reward(i)` as:

```
estimated_surprisal(i) = estimated_reward(i) - discount * estimated_reward(i+1)
marginal_advantage(i) = surprisal(i) - estimated_surprisal(i)
advantage(i) = marginal_advantage(i) + discount * marginal_advantage(i+1) + discount^2 * marginal_advantage(i+2) + ...
```

Unlike with rewards, we don't train an estimator so the advantage is calculated from the limited number of surprisals available. So advantages for the last few states will be less accurate: The advantage for the final state in our memory will not be accounting for future advantages at all. It would be reasonable to discard the last few states during training, although we chose to include them.

### Policy (Proximal Policy Optimization)

This optimization algorithm was published by OpenAI, and has been used in recent reinforcement learning results such as [defeating professional teams in Dota 2](https://openai.com/blog/how-to-train-your-openai-five/).

![Policy Network](/assets/articles/20190512-marai/policy-network.svg)

The three convolutional and dense layers convert a game state into a preference score for each of the 14 actions. The Softmax Cross Entropy normalizes the scores so they sum to 1. The scores are effectively probabilities, though they are only converted to a [0, 1] range at the end.

A vanilla [Policy Gradient (PG)](TODO: insert link) tries to maximize the value `action_score * advantage`.[^3] PG requires finely tuned learning rates: Too low and PG is already sample-inefficient so it will take a long time to converge; too high and naive policy updates may be large enough to completely change the states the agent visits. PG can't effectively learn from the same rollout multiple times, because this magnifies the changes even further - making it difficult to learn.

PPO clamps updates that result in large changes to the action score, so it 1. Is more robust to the learning rate and 2. Can train multiple times on the same data, increasing sample efficiency.

When the agent is playing the game, it remembers its action score for every (state_i, action_i) encountered. The agent is updated during play, so it can compare its new policy's scores to the old scores to ensure it doesn't change too much.

```
A = action_i
S = state_i
P = score of selecting action A in state S
R = remembered score of selecting action A in state S
ratio = P / R

pg_loss = max(-advantage * ratio, -advantage * clip(ratio, 0.9, 1.1))
```

The remembered score `R` and the advantage are effectively constants: The neural network can only change its current action score `P`. Let's look at how PPO's `pg_loss` differs from the unmodified loss `-advantage * ratio`.

If ratio is between 0.9 and 1.1, then both arguments to `max` are equal so nothing changes. Otherwise, there are 4 cases to consider, depending on the sign of `advantage` and whether `ratio` is above or below its bounds.

| Case | ratio | Adv | -adv * ratio | -adv * clip(ratio) | loss |
| --- | --- | --- | --- | --- | --- |
| Reinforce Optimism | 1.2 | 1 | -1.2 | -1.1 | -1.1 (clipped) |
| Reject Pessimism | 0.8 | 1 | -0.8 | -0.9 | 0.8 (unclipped) |
| Reject Optimism | 1.2 | -1 | 1.2 | 1.1 | 1.2 (unclipped) |
| Reinforce Pessimism | 0.8 | -1 | 0.8 | 0.9 | -0.9 (clipped) |

Updates cause the score (and ratio) to increase when advantage is positive, and decrease when the advantage is negative. The updates are also proportional to the magnitude of the loss.

Reinforce cases occur when the network agrees with previous minibatches, updating the ratio away from 1.0. Reject cases disagree with previous minibatches, pushing the score back towards the remembered score. The Reinforce cases are clipped, the Reject cases are not.

### Entropy

When the Policy network initializes, its preferences and errors are random and small. The entropy loss prevents the network from strengthening these initial preferences by chance:

$$
\begin{align}
score_i &= e^{advantage_i} \\
total &= \sum_i score_i \\
p_i &= score_i / total \; \; \text{Percentage of rescaled score} \\
\\
entropy_{loss} &= (- 0.001) * \sum_i p_i * (log(total) - advantage_i)
\end{align}
$$

Entropy is minimized when every action is equally preferred, and increases as the network develops stronger preference for certain actions over others.

As the network trains, the `reward_loss` and `pg_loss` will increase by orders of magnitude while the `entropy_loss` stays roughly the same. So the effect of entropy diminishes as the network learns.

## Conclusion

This article explained how we replicated an OpenAI paper to train an agent that learned to play the original Super Mario Bros.

Existing techniques work by densely sampling a random exploration space. They don't plan or reason about the world - they make random movements and remember what seems to work.

The next article here will explore training an agent that trains a model of a symbolic temporal logic, so that there are interpretable reasons for choosing certain actions over others.

### Footnotes

[^1]: Levels 4-4 and 8-4 require the player to take a specific route, or else is reset to an earlier point in the level without an obvious screen transition.
[^2]: Both [OpenAI Gym](https://gym.openai.com/) and a [Custom Emulator](https://www.michaelburge.us/2019/03/18/nes-design.html) are supported targets. All video footage was recorded with the custom emulator.
[^3]: Maximizing `action_score * advantage` is the same as minimizing `action_score * -advantage`, which is what you see in the graph. Optimizers are typically written to minimize a value.
[^4]: In our case, we choose discount = 0.99.
