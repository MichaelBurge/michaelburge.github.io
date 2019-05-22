---
layout: post
author: Tian Qi and Michael Burge
title: "Curiosity Killed the Mario"
date: 2019-05-21 20:00
tags:
  - machine_learning
  - python
  - rust
js_files:
  - https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML

---

[Super Mario Bros](https://en.wikipedia.org/wiki/Super_Mario_Bros.) is a video game released for the [Nintendo Entertainment System(NES)](https://en.wikipedia.org/wiki/Nintendo_Entertainment_System) in 1985. We replicate a paper from members of [OpenAI and UC Berkeley](https://arxiv.org/abs/1808.04355), creating agents that learn to solve many levels in this game[^1].

The central idea is to use **Curiosity** as a reward. The agent
* Attempts to predict future states
* Prefers to enter states that it's unable to predict well.

In Mario, this reward encourages moving right or finding bonus rooms, which is often sufficient to complete a level.

<!-- -->
Table of Contents
* This list is replaced with the Table of Contents during page generation
{:toc}

## Results

We recorded a video with the furthest progress made on an interesting subset of levels below:

<iframe width="560" height="315" src="https://www.youtube.com/embed/_cn1TDHqikY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Each level trained over a maximum of 350 hours of gameplay. This is broken up into 50,000 **Episodes**, each 1536 frames in length. If the agent ever passes the level, the video records the fastest passing run. Otherwise, we used the run with the most progress. Mario was given 3 lives each run, but the video is edited to remove lives that die and don't reach a level checkpoint.

A custom emulator runs at more than 60 FPS and episodes run in parallel on a server. It would take two weeks to test every Mario level, so only the interesting levels were tried. The agents were trained from scratch on every level, so no learning is transferred from earlier levels to later levels.

| Level | Pass? | Description |
| ---   | ---   | --- |
| [1-1](https://www.youtube.com/watch?v=_cn1TDHqikY&t=0s)   | <span style="green">Yes</span> | This level is consistently beaten. It takes less time if it finds a hidden room that skips most of the level. |
| [1-2](https://www.youtube.com/watch?v=_cn1TDHqikY&t=41s) | <span style="green">Yes</span> | A narrow section requires Mario to wait until enemies pass through, contrary to the general policy of “moving right”. The agent consistently passes this level, rarely finding the [Warp Zone](https://www.mariowiki.com/Warp_Zone). |
| [1-3](https://www.youtube.com/watch?v=_cn1TDHqikY&t=92s) | <span style="red">No</span> | It completes most of the level. It's possible the different tileset at the end confuses it. |
| [1-4](https://www.youtube.com/watch?v=_cn1TDHqikY&t=144s) | <span style="green">Yes</span> | The first Bowser castle introduces spinning traps and Bowser. |
| [2-2](https://www.youtube.com/watch?v=_cn1TDHqikY&t=190s) | <span style="Yellow">Yes</span> | The first water level uses a different movement mechanic and new [enemies](https://www.mariowiki.com/blooper). The agent rarely passes the level. It struggles against choke points guarded by Bloopers. |
| [2-3](https://www.youtube.com/watch?v=_cn1TDHqikY&t=360s) | <span style="red">No</span> | Mario crosses a long bridge while being constantly assaulted by random flying fish |
| [2-4](https://www.youtube.com/watch?v=_cn1TDHqikY&t=457s) | <span style="Yellow">Yes</span> | A Bowser castle. An obstacle at the end causes the agent to often bump into Bowser. The agent rarely beats this level. |
| [3-4](https://www.youtube.com/watch?v=_cn1TDHqikY&t=523s) | <span style="Yellow">Yes</span> | A Bowser castle. The agent frequently dies to Bowser, because of another obstacle. |
| [4-1](https://www.youtube.com/watch?v=_cn1TDHqikY&t=583s) | <span style="green">Yes</span>| A new enemy Lakitu follows Mario and throws monsters at him. The agent consistently beats this level. |
| [4-2](https://www.youtube.com/watch?v=_cn1TDHqikY&t=638s) | <span style="green">Yes</span> | This level has 3 possible exits: A vine, a warp zone, and the flag. The agent finds the warp zone and flag. |
| [4-4](https://www.youtube.com/watch?v=_cn1TDHqikY&t=705s) | <span style="red">No</span>| A non-linear level that requires Mario to follow a specific route, or else he's reset to an earlier location |
| [8-1](https://www.youtube.com/watch?v=_cn1TDHqikY&t=828s) | <span style="red">No</span>| There is a gap that requires a very long jump to cross, which is too precise for the agent |
| [8-3](https://www.youtube.com/watch?v=_cn1TDHqikY&t=894s) | <span style="red">No</span> | The level contains an assortment of enemies. The Bullet Bills and Hammer Bros often kill the agent. |
| [8-4](https://www.youtube.com/watch?v=_cn1TDHqikY&t=938s) | <span style="red">No</span> | The level contains a water section and non-linear map. Like 4-2, Mario must follow a specific route. |

Maps for each level are available [here](https://nesmaps.com/maps/SuperMarioBrothers/SuperMarioBrothers.html), and code used to train the agent is on [Github](https://github.com/D8ms/public_marai).

There are interesting behaviors common to all levels:

* The agent alternates between running and sprinting (Holding B). The variation in speed makes future states harder to predict.

* It can degenerate into a policy that stays still or gets stuck at a wall. These usually resolve with more training.

* After first completing a level, the agent does not consistently pass. There is no explicit reward for doing so, and it may temporarily expect higher surprisals in paths that do not complete the level.

## Concepts

A curious agent explores and understands its environment. "Understand" means predicting its future environment. "Exploring" means preferring actions that lead to states with incorrect future predictions - states with high **Surprisal**.

The environment has an associated **State**. The state is the set of inputs the network can use to make a decision. In general, this could include the pixels in a frame, recently generated audio, or text mapped with an embedding. We'll define the state later.

Each state has a set of associated **Moves**. A move causes the environment to transition to a different state. There are 14 moves the Mario agent chooses from. This subset prevents it from pausing or pressing left/right at the same time. They are:

* NOTHING
* UP, DOWN, LEFT, RIGHT
* A, B, A + B
* RIGHT + {A, B, A + B}
* LEFT + {A, B, A + B}

Given a state, the agent should choose the **Move** that provides the biggest **Reward**. Reward is not the same as surprisal. The surprisal is the unexpectedness of a single state, but reward factors in future surprisals as well.

The trained agent plays Mario by repeatedly:
* Retrieving the current state from the NES emulator[^2]
* Sending the state to the **Policy Network** to get a recommended action
* Sending the action to the emulator

### State

The state is not a single frame. In Mario, it's important to hold the jump button to make a higher jump. So the agents action spans 12 frames. We collect the 4th, 8th and 12th frames; resize, crop, and grayscale them to size 84x84; and then stack them to create the 84x84x3 state.

The resized frames have a width and height of 84 and 110. The top 18 and bottom 8 pixels are cropped: The top 18 pixels only contain the score, lives, and coin counter; and the bottom 8 pixels contain nothing of value. The NES has a low number of colors, so grayscaling doesn't drop important information and reduces the size of the input.

We choose to use stacked frames rather than single frames for these reasons:
* The Mario sprite frequently changes while walking. Our network is not recurrent, so it has no memory of the past. So it has no way to determine if the sprite will change in the next frame, and will always be surprised. The 4-frame gap causes the sprite to change every sample, so it is not surprising.
* The sampling gap increases the feedback from the chosen action. The difference between moving right for 1 frame and 15 frames is easier to see. So the agent more quickly prefers moving right.
* Many pipes and gaps require a full jump to pass. The agent must hold jump for at least 30 frames. This action sequence is unlikely to be randomly chosen. With a 12-frame span, the agent only needs to hold jump for 2-4 states.
* 3 frames allows our agent to approximate Mario’s velocity and acceleration. The velocity distinguishes between rising and falling sprites. The acceleration might be helpful determining whether it can jump over obstacles.
* The OpenAI paper stacked 4 frames with 3 frame gaps. We use a 3-stack to help visualize the state: Each greyscale frame can be assigned to the Red, Green, or Blue color channels. It didn't seem to decrease performance.

After the state is created, it is **Normalized** by subtracting the **mean state** and dividing by the **mean std**([**Standard Deviation**](https://en.wikipedia.org/wiki/Standard_deviation)). This gives better results than another common way of normalizing pixels: Subtracting by 127.5 and dividing by 255.

The mean state and mean std are calculated from the collected states after making 10,000 random moves at the beginning of the level. Each pixel has its own mean, but the standard deviation is averaged to produce a single number.

The means of the pixels can be combined to form an image:

* Level 1-1: ![Mean State 1-1](/assets/articles/20190512-marai/mean-state-1.png)
* Level 8-4: ![Mean State 8-4](/assets/articles/20190512-marai/mean-state-2.png)

### Surprisal vs. Reward

An action changes a state. All differences between the old and new state can be attributed to:

* Level change and black screens from death or level transition
* Camera scrolling
* Sprite moving or changing

and this is ordered from largest to smallest expected change. Larger changes are harder to predict, so a level change is more interesting than a slightly displaced Goomba.

Dying gives a large surprisal, comparable to entering a new level: The screen suddenly goes black. This is followed by another large surprisal when the level restarts. So using surprisal as a reward would encourage the agent to choose avoidable death. This is undesirable, so we use a different reward.

The reward for an initial state $$\text{state}(0)$$ is defined inductively as:
* The environment is measured as $$\text{state}(0)$$
* The model produces an action $$\text{action}(0)$$ and an expected state $$\text{guess}(0)$$
* $$\text{action}(0)$$ is sent to the environment to advance it to $$\text{state}(1)$$
* The difference between $$\text{state}(1)$$ and $$\text{guess}(0)$$ is $$\text{surprisal}(0)$$

This can be repeated with $$\text{state}(1)$$ to produce $$\text{surprisal}(1)$$. Iteration of this process defines an infinite stream of surprisals.

The ideal reward is the infinite sum:

$$
\begin{split}
\text{discount} &= 0.99 \\
\text{reward}_{\text{ideal}}(i) & = \text{discount}^0 * \text{surprisal}(i) + \text{discount}^1 * \text{surprisal}(i+1) + ... \\

&= \sum_{k=0}^\infty \text{discount}^k * \text{surprisal}(i+k)
\end{split}
$$

The model must be fixed for these surprisals to be well-defined, since the states depend on the actions taken. Since $$\text{discount}^n$$ converges to 0 as $$n$$ increases, distant surprisals have less impact than closer ones.

We can't keep infinite states and guesses in memory, so the actual reward is an approximation - see the [Reward](#reward) section for details.

This ideal reward doesn't encourage the network to die as much. It still gets a large surprisal on death, but since it reenters the same level all other surprisals will be close to zero. So the strategy of "moving right" is preferred over dying, and in Mario that is usually enough to beat the level.

## Model

The model is two distinct networks: **Policy** and **Dynamics**. Dynamics can be further broken down into **Inverse Dynamics** and **Forward Dynamics**.

Policy
* Estimates the expected reward for a given state
* Provides a **Preference Score**(score) for the 14 actions at a given state.
* Chooses one of the actions. This is usually the action with the highest score, but there is some randomnesss.
* During training, prevents the **Trajectory** from changing too fast. The trajectory is the sequence of states produced by following this policy.

Inverse Dynamics
* A [**Convolutional Neural Network**(CNN)](https://en.wikipedia.org/wiki/Convolutional_neural_network) maps states to **Feature Vectors**. A feature vector is 512 floating point numbers, each weighing an abstract pixel texture developed from training. This CNN is named the **Feature Mapper**.
  * The Feature Mapper is an encoder for states. The network does not predict the next state: it predicts the next state as encoded by the Feature Mapper. The network decides what is important in a state.
* The inputs to this network are two consecutive states. Both states are mapped to feature vectors, and the output is a prediction of which action was taken.
* Since we provide two consecutive states, we already know which action was taken. The true purpose of this network is to train the Feature Mapper.

Forward Dynamics
* Predicts the feature vector of the next state, from the current state and an action.
* The surprisal is the [L2 difference](https://en.wikipedia.org/wiki/Norm_(mathematics)#Euclidean_norm) between the feature vector predicted here and the actual feature vector.
* The Feature Mapper is prevented from training. This prevents the network from learning a degenerate mapping like "the feature vector is always zero", which is very easy to predict. This training of the Feature Mapper is the main reason for the Inverse/Forward split.


The Policy and Dynamics networks act as an [Actor-Critic Pair](https://lilianweng.github.io/lil-log/2018/04/08/policy-gradient-algorithms.html#actor-critic). The Critic tells the Actor how good or bad the move it made was, which the Actor uses to adjust action preferences.

### Training and Rollout

The agent alternates between playing and training. It makes 128 moves to collect data, which is then used to train the network. These continuous segments of play are **Episodes**.

A **Trajectory** is the collection of $$(\text{state}, \text{action})$$ pairs gathered during a single episode.

A **Rollout** is the trajectory along with many metrics calculated from the collected information, such as the $$\text{surprisal}(i)$$ values.

Since the surprisal is defined using the next state we actually need 129 states as input. The very last state is called the **tail state** and is not trained on.

Because the policy includes a small amount of randomness, we can run multiple Mario environments in parallel to collect diverse data. All environments use the same model to infer action and metrics. During training, we repeatedly sample rollouts as mini-batches. We used 8 parallel environments, one rollout per mini-batch, and we train on all 8 mini-batches 3 times for a total of 24 mini-batches per episode. Training only uses the rollout from the current episode, so we don’t need to keep the rollouts for future updates.

If the batches only contain a small subsection of the level, then the network update can cause the policy to become ill-fit to pass the parts of the level that were not seen in the batch. As a result, performance increases with bigger batches. The original paper uses 128 to 2048 parallel environments and noticeably outperforms our experiment.


### Dynamics

<img src="/assets/articles/20190512-marai/dynamics-network.svg" class="center">

This network contains a CNN called the Feature Mapper that converts states into feature vectors.

Inverse Dynamics concatenates each state's feature vector with the subsquent state's feature vector, feeds that through two Dense layers, and uses a [Softmax Cross Entropy](https://www.tensorflow.org/api_docs/python/tf/nn/softmax_cross_entropy_with_logits) to decide which action was taken. The actual action is used as input to calculate the loss. As mentioned earlier, the purpose of Inverse Dynamics is to train the Feature Mapper.

Forward Dynamics attempts to predict the feature vector of the next state given the current state and an action. It puts the feature vector through multiple [Residual Layers](https://en.wikipedia.org/wiki/Residual_neural_network). The action is represented as a [One-hot Vector](https://en.wikipedia.org/wiki/One-hot). The final Dense layer has 512 features, representing the predicted features of the next state. The loss of Forward Dynamics is the L2 difference between the predicted and actual feature vectors of the next state. This loss is known as the surprisal.

The `Dense Resnet Layer` node was implemented in two different ways: OpenAI's original code used the Bottom-Left component in the Dynamic Graph, while we used the Bottom-Right component. As a result, our "Dense Residual Network" is not actually residual. This was an error that improved performance on level 1-1, so we kept it.

### Policy

<img src="/assets/articles/20190512-marai/policy-network.svg" class="center">

The Policy network looks like it has a Feature Mapper of its own, but it's trained independently - it isn't shared with the Dynamics network.

The three loss functions $$\text{reward_loss}$$, $$\text{entropy_loss}$$, and $$\text{pg_loss}$$ are used to train the network.

#### Estimated Reward

$$\text{reward_loss}$$ is used to train an approximator to the ideal reward. The rollout contains 129 states, surprisals, and rewards estimated from the Policy network. The ideal reward is defined as:

$$
\text{reward}_\text{ideal}(i) = \sum_{k=0}^\infty \text{discount}^k * \text{surprisal}(i+k)
$$

We use Forward Dynamics to get 128 surprisals from the 129 states and semi-normalize them by dividing against a running standard deviation. This reduces variance and stabilizes training. We keep one running std per environment.

The definition of $$\text{reward}_\text{ideal}$$ requires infinitely many surprisals, but there are a limited amount available - especially for the last states. We work around this by bootstrapping using an estimated reward: The Policy Network estimates the true reward, and we combine that with the 128 surprisals to calculate a target reward. So the reward has a somewhat circular definition:

$$
\begin{align}
\text{target_reward}(i) &= \text{surprisal}(i) + \text{discount} * \text{predicted_reward}(i+1) \\
\text{reward_loss}(i) &= 0.5 * (\text{target_reward}(i) - \text{predicted_reward}(i))^2
\end{align}
$$

As the network trains, $$\text{predicted_reward}$$ and $$\text{target_reward}$$ should converge to each other. A corollary is that $$\text{predicted_reward}(i) - \text{discount} * \text{predicted_reward}(i+1)$$ should converge to $$\text{surprisal}(i)$$.

#### Entropy

When the Policy network initializes, its preferences and errors are random and small. The entropy loss prevents the network from strengthening these initial preferences by chance:

$$
\begin{align}
\text{score}(i) &= e^{\text{advantage}(i)} \\
\text{total} &= \sum_i \text{score}(i) \\
p(i) &= \text{score}(i) / \text{total} \; \; \text{-- Percentage of rescaled score} \\
\\
\text{entropy_loss} &= (- 0.001) * \sum_i p(i) * (\text{log}(\text{total}) - \text{advantage}(i))
\end{align}
$$

Entropy is minimized when every action is equally preferred, and increases as the network develops stronger preference for certain actions over others.

As the network trains, the $$\text{reward_loss}$$ and $$\text{pg_loss}$$ will increase by orders of magnitude while the $$\text{entropy_loss}$$ stays roughly the same. So the effect of entropy vanishes as the network learns.

#### Proximal Policy Optimization(PPO)

PPO is an optimization algorithm published by OpenAI, and has been used in recent reinforcement learning results such as [defeating professional teams in Dota 2](https://openai.com/blog/how-to-train-your-openai-five/).

The network assigns each of the 14 possible actions an $$\text{action_score}$$. These scores are converted into probabilities when an action needs to be randomly sampled.

The Advantage Function $$\text{A}(\text{state}, \text{action})$$ gives the benefit of the action relative to other actions available at that state. It is positive if the action is better than average, and negative if worse.

The related $$\text{advantage}(i) = \text{A}(\text{state}(i), \text{action}(i))$$ gives the advantage of the action actually chosen at state $$i$$ of the rollout. It is defined using rollup similar to $$\text{reward}(i)$$ as:

$$
\begin{align}
\text{predicted_surprisal}(i) &= \text{predicted_reward}(i) - \text{discount} * \text{predicted_reward}(i+1) \\
\text{marginal_advantage}(i) &= \text{surprisal}(i) - \text{predicted_surprisal}(i) \\
\text{advantage}(i) &= \sum_{k=0}^\infty \text{discount}^k * \text{marginal_advantage}(i+k)
\end{align}
$$

Because $$\text{advantage}(i)$$ is calculated over the actual path, there are only 128 surprisals needed. $$\text{A}(state, action)$$ is not efficiently computable in general, because it would need surprisals to be computed over all $$14^{128}$$ paths[^5]. So $$\text{advantage}(i)$$ is an approximation of $$\text{A}$$.

Unlike with rewards, we don't train an estimator so the advantage is calculated from the limited number of surprisals available. So advantages for the last few states will be less accurate: The advantage for the final state in our memory will not be accounting for future advantages at all. It would be reasonable to discard the last few states during training, although we chose to include them.

A vanilla Policy Gradient (PG) tries to maximize the value $$\text{action_score} * \text{advantage}$$.[^3] PG requires finely tuned learning rates: Too low and PG is already sample-inefficient so it will take a long time to converge; too high and naive policy updates may be large enough to completely change the states the agent visits. PG can't effectively learn from the same rollout multiple times, because this magnifies the changes even further - making it difficult to learn.

PPO clamps updates that result in large changes to the action score, so it is more robust to the learning rate and can train multiple times on the same data, increasing sample efficiency.

When the agent is playing the game, it remembers its action score for every $$(\text{state}, \text{action})$$ encountered. The agent is updated during play, so it can compare its new policy's scores to the old scores to ensure it doesn't change too much.

$$
\begin{align}
\text{A} &= \text{action}(i) \\
\text{S} &= \text{state}(i) \\
\text{P} &= \text{score of selecting action A in state S} \\
\text{R} &= \text{remembered score of selecting action A in state S} \\
\text{ratio} &= \text{P} / \text{R} \\

\text{pg_loss} &= \text{max}(-\text{advantage} * \text{ratio}, -\text{advantage} * \text{clip}(\text{ratio}, 0.9, 1.1))
\end{align}
$$

The remembered score $$\text{R}$$ and the advantage are effectively constants: The neural network can only change its current policy - the value $$\text{P}$$. Let's look at how PPO's $$\text{pg_loss}$$ differs from the unmodified loss $$-\text{advantage} * \text{ratio}$$.

If ratio is between 0.9 and 1.1, then both arguments to $$\text{max}$$ are equal so nothing changes. Otherwise, there are 4 cases to consider, depending on the sign of $$\text{advantage}$$ and whether $$\text{ratio}$$ is above or below its bounds.

| Case | ratio | Adv | -adv * ratio | -adv * clip(ratio) | loss |
| --- | --- | --- | --- | --- | --- |
| Reinforce Optimism | 1.2 | 1 | -1.2 | -1.1 | -1.1 (clipped) |
| Reject Pessimism | 0.8 | 1 | -0.8 | -0.9 | 0.8 (unclipped) |
| Reject Optimism | 1.2 | -1 | 1.2 | 1.1 | 1.2 (unclipped) |
| Reinforce Pessimism | 0.8 | -1 | 0.8 | 0.9 | -0.9 (clipped) |

Updates cause the score (and ratio) to increase when advantage is positive, and decrease when the advantage is negative. The updates are also proportional to the magnitude of the loss.

Reinforce cases occur when the network agrees with previous minibatches, updating the ratio away from 1.0. Reject cases disagree with previous minibatches, pushing the score back towards the remembered score. The Reinforce cases are clipped, the Reject cases are not.

## Conclusion

This article explained how we replicated a paper to train agents to play many levels of the original Super Mario Bros.

A future article hopes to discuss some original research on the same topic. Existing techniques work by densely sampling a random exploration space. They don't plan or reason about the world - they make random movements and remember what seems to work. By training the agent to fit a symbolic temporal logic, we can guarantee that there are interpretable reasons for choosing certain actions over others.

One example of why this could be useful: A self-driving car usually needs to stop at a red light, but if the car behind you is speeding then stopping may be dangerous. If the police ticket you, you'd want the car to provide an explicit reason that it ran the red light.

### Footnotes

[^1]: You might also like OpenAI's post [here](https://openai.com/blog/reinforcement-learning-with-prediction-based-rewards/).
[^2]: Both [OpenAI Gym](https://gym.openai.com/) and a [Custom Emulator](https://www.michaelburge.us/2019/03/18/nes-design.html) are supported targets. All video footage was recorded with the custom emulator.
[^3]: Maximizing $$\text{action_score} * \text{advantage}$$ is the same as minimizing $$\text{action_score} * -\text{advantage}$$, which is what you see in the graph. Optimizers are typically written to minimize a value.
[^5]: $$\text{A}(\text{state}, \text{action})$$ can be approximately calculated over a single rollout using $$14^{128}$$ paths, or ideally over the infinite tree of all possible paths. They're both equally feasible.