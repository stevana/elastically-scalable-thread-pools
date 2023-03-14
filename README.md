# Elastically scalable thread pools

An experiment in controlling the size of a thread pool using a PID controller.

## Motivation

A tried and tested way to achieve parallelism is to use pipelining. It's used
extensively in manufacturing and in computer hardware.

For example, Airbus [apparently](https://youtu.be/oxjT7veKi9c?t=2682) outputs
two airplanes per day on average, even though it takes two months to build a
single airplane from start to finish. It's also used inside CPUs to [pipeline
instructions](https://en.wikipedia.org/wiki/Instruction_pipelining).

Let's imagine we want to take advantage of pipelining in some software system.
To make things more concrete, let's say we have a system where some kind of
requests come on over the network and we want to process them in some way. The
first stage of the pipeline is to parse the incoming requests from raw
bytestrings into some more structured data, the second stage is to apply some
validation logic to the parsed data and the third stage is to process the valid
data and produce some outputs that are then sent back to the client or stored
somewhere.

```mermaid
  flowchart LR
    Q1(Queue of bytestrings) --> S1((Parse)) --> Q2(Queue of data)
    Q2--> S2((Validate)) --> Q3(Queue of valid data)
    Q3--> S3((Process)) --> Q4(Queue of outputs)
```

The service time of an item can differ from stage to stage, for example parsing
might be slower than validation, which can create bottlenecks. Luckily it's
quite easy to spot bottlenecks by merely observing the queue lengths and once a
slow stage is found we can often fix it by merely adding an additional parallel
processor to that stage. For example we could spin up two or more threads that
take bytestrings from the first queue and turn them into structured data and
thereby compensate for parsing being slow.

By spinning up more threads we can decrease latency (waiting time in the queue)
and increase throughput (process more items), but we are also on the other hand
using more energy and potentially hogging CPU resources that might be better
used elsewhere in the pipeline or system at large.

So here's the question that the rest of this post is concerned about: can we
dynamically spin up and spin down threads at a stage in response to the input
queue length for that stage?

## Plan

Let's focus on a single stage of the pipeline to make things easier for
ourselves.

```mermaid
flowchart LR
    I(Input queue) --> P((Processor)) --> O(Output queue)
```

We'd like to increase the parallelism of the processors if the input queue
grows, and decrease it when the queue shrinks. One simple strategy might be to
establish thresholds, i.e. if there's over $100$ items in the input queue then
allocate more processors and if there's no items in the queue then deallocate
them.

Since allocating and deallocating processors can be an expense in itself, we'd
like to avoid changing them processor count unnecessarily.

The threshold based approach is sensitive to unnecessarily changing the count if
the arrival rate of work fluctuates. The reason for this is because it only
takes the *present* queue length into account.

We can do better by also incorporating the *past* and trying to predict the
*future*, this is the basic idea of [PID
controllers](https://en.wikipedia.org/wiki/PID_controller) from [control
theory](https://en.wikipedia.org/wiki/Control_theory).

Here's what the picture looks like with a PID controller in the loop:


```
                                            +----------------------------------+
                                            |                                  |
    ------------------------------------------->[Input queue]-->[Worker pool]----->[Output queue]-->
                                            |                                  |
     r(t)   e(t)                    u(t)    |                                  |
    ----->+------>[PID controller]--------> |                                  |
          ^                                 |                                  |
          |                                 +----------------------------------+
          |                                                 | y(t)
          +-------------------------------------------------+

```

The PID controller monitors the queue length $y(t)$, compares it to some desired
queue length $r(t)$ (also known as the setpoint) and calculates the error $e(t)$.
The error determines the control variable $u(t)$ which is used to grow or shrink
the processor pool.

## Pseudo-code

Let's start top-down with the `main` function which drives our whole experiment.

### Main

```
main =

  // Create the in- and out-queues.
  inQueue  := newQueue()
  outQueue := newQueue()


  // The workers don't do anything interesting, they merely sleep for a bit to
  // pretend to be doing some work.
  worker := sleep 0.025s

  // Create an empty worker pool.
  pool := newPool(worker, inQueue, outQueue)

  // Start the PID controller in a background thread. The parameters provided
  // here allow us to tune the PID controller, we'll come back to them later.
  kp := 1
  ki := 0.05
  kd := 0.05
  dt := 0.01s
  fork(pidController(kp, ki, kd, dt, pool))


  // Create a workload for our workers. We use the sine function to create
  // between 0 and 40 work items every 0.1s for 60s. The idea being that because
  // the workload varies over time the PID controller will have some work to do
  // figuring out how many workers are needed.
  sineLoadGenerator(inQueue, 40, 0.1s, 60s)
```

### Worker pool

The worker pool itself is merely a struct which packs up the necessary data we
need to be able to scale it up and down.

```
struct Pool =
  { inQueue:  Queue<Input>
  , outQueue: Queue<Output>
  , worker:   Function<Input, Output>
  , pids:     List<ProcessId>
  }
```

Creating a `newPool` creates the struct with an empty list of process ids.

```
newPool worker inQueue outQueue = Pool { ..., pids: emptyList }
```

Scaling up and down are functions that take and return a `Pool`.

```
scaleUp pool =
  work := forever
            x := readQueue(pool.inQueue)
            y := pool.worker(x)
            writeQueue(pool.outQueue, y)
  pid   := fork(work)
  pool' := pool.pids = append(pid, pool.pids)
  return pool'
```

The function `scaleDown` does the inverse, i.e. kills and removes the last
process id from `pool.pids`.

### Load generator

In order to create work load that varies over time we use the sine function. The
sine function oscillates between $-1$ and $1$:

![](img/sine.svg)

We would like to have it oscillate between $0$ and some max value $m$. By
multiplying the output of the sine function by $m/2$ we get an oscillation
between $-m/2$ and $m/2$, we can then add $m/2$ to make it oscillate between $0$
and $m$.

We'll sample the resulting function once every `timesStep` seconds, this gives
us the amount of work items (`n`) to create we then spread those out evenly in
time, rinse and repeat until we reach some `endTime`.

```
sineLoadGenerator inQueue workItem maxItems timeStep endTime =
  for t := 0; t < endtime; t += timeStep
    n := sin(t) * maxItems / 2 + maxItems / 2
    for i := 0; i < n; i++
      writeQueue(inQueue, workItem)
      sleep(timeStep / n)
```

### PID controller

The PID controller implementation follows the pseudo-code given at
[Wikipedia](https://en.wikipedia.org/wiki/PID_controller#Pseudocode):

```
previous_error := 0
integral := 0
loop:
   error := setpoint − measured_value
   proportional := error;
   integral := integral + error × dt
   derivative := (error − previous_error) / dt
   output := Kp × proportional + Ki × integral + Kd × derivative
   previous_error := error
   wait(dt)
   goto loop
```

Where `Kp`, `Ki` and `Kd` is respectively the proportional, integral and
derivative gain and `dt` is the loop interval time. The proportional part acts
on the *present* error value, the integral acts on the *past* and the derivative
tries to predict the *future*. The measured value is the input queue length and
the setpoint, i.e. desired queue length, is set to zero. If the `output` of the
PID controller is less than $-100$ (i.e. the queue length is over $100$ taking
the present, past and possible future into account) then we scale up and if it's
more than $-20$ (i.e. the queue length is less than $20$) then we scale down the
worker pool.

## How it works

We start off by only setting the proportional part and keeping the integral and
derivative part zero, this is called a P-controller. We see below that it will
scale the worker count up and down proportionally to the sine wave shaped load:

![](img/elastically-scalable-thread-pools-1.0-0.0-0.0.svg)

A P-controller only focuses on the *present*, and we see that it allocates and
deallocates workers unnecessarily. In order to smooth things out we introduce
the integral part, i.e. a PI-controller. The integral part takes the *past* into
account. We see now that the worker count stabilises at $28$:

![](img/elastically-scalable-thread-pools-1.0-5.0e-2-0.0.svg)

We can improve on this by adding the derivative part which takes the *future*
into account. We then see that it stabilises at $26$ workers:

![](img/elastically-scalable-thread-pools-1.0-5.0e-2-5.0e-2.svg)

With the full PID controller, which stabilises using less workers than the
PI-controller, we see that the queue length spikes up to $20$ or so each time
the work load generator hits one of the sine function's peaks. Recall that we
started scaling down once the queue length was less than $20$.

## Usage

The above graphs were generated by running: `cabal run app -- kp ki kd`, where
the $K_p$, $K_i$, and $K_d$ parameters are the tuning parameters for the
PID controller.

If you don't have the GHC Haskell compiler and the `cabal` build tool already
installed, then the easiest way to get it is via
[`ghcup`](https://www.haskell.org/ghcup/). Alternatively if you got `nix` then
`nix-shell` should give give you access to all the dependencies you need.

## Contributing

There are many ways we can build upon this experiment, here are a few ideas:

- [ ] We probably want to limit the max number of threads in a pool;
- [ ] If two or more threads take items from some input queue and put them on
      some output queue then there's no guarantee that the order of the output
      items will be the same as the input items. We could solve this, and regain
      determinism, by using array based queues and shard on the index, i.e. even
      indices goes to one processor and odd to an other or more generally
      modulus N can be used to shard between N processors. This is essentially
      what the [LMAX
      Disruptor](https://en.wikipedia.org/wiki/Disruptor_(software)) does;
- [ ] We've only looked at one stage in a pipeline, what happens if we have
      multiple stages? is it enough to control each individual stage separately
      or do we need more global control?
- [ ] Can we come up with other things to control? E.g. batch sizes?
- [ ] We've only monitored the current queue length, could we combine this with
      other data? E.g. time series of the queue length from the previous day?
- [ ] Is it robust to wildly changing usage patterns? E.g. bursty traffic or the
      [Slashdot effect](https://en.wikipedia.org/wiki/Slashdot_effect)?
- [ ] We've looked at scaling up and down on a single machine (vertical
      scaling), what about scaling out and in across multiple machines
      (horizontal scaling)?
- [ ] We generated and processed real work items (by sleeping), could we do a
      discrete-event simulation instead to avoid having to wait for the sleeps?
- [ ] I just picked random values for the PID controller parameters, there are
      more principled
      [ways](https://en.wikipedia.org/wiki/PID_controller#Overview_of_tuning_methods)
      of tuning the PID controller;
- [ ] The PID controller we implemented merely followed the pseudo-code from
      Wikipedia, there's probably better ways of implementing it?

If any of this sounds interesting, feel free to get in touch!

## See also

* [*A Review of Auto-scaling Techniques for Elastic Applications in Cloud
  Environments*](https://www.researchgate.net/publication/265611546_A_Review_of_Auto-scaling_Techniques_for_Elastic_Applications_in_Cloud_Environments)
  (2014) is a survey paper which talks about both threshold and PID controllers;

* [*SEDA: An Architecture for Well-Conditioned Scalable Internet
  Services*](https://people.eecs.berkeley.edu/~brewer/papers/SEDA-sosp.pdf)
  (2001), this is paper that I got the idea for elastic scalable thread pools.
  They use a threshold approach rather than a PID controller, saying:

  > The controller periodically samples the input queue (once per second by
  > default) and adds a thread when the queue length exceeds some threshold (100
  > events by default). Threads are removed from a stage when they are idle for a
  > specified period of time (5 seconds by default).

  But also:

  > Under SEDA, the body of work on control systems can be brought to bear on
  > service resource management, and we have only scratched the surface of the
  > potential for this technique.

  A bit more explanation is provided by Matt Welsh, who is one of the author, in
  his PhD
  [thesis](https://cs.uwaterloo.ca/~brecht/servers/readings-new/mdw-phdthesis.pdf)
  (2002):

  > A benefit to ad hoc controller design is that it does not rely on complex
  > models and parameters that a system designer may be unable to understand or to
  > tune. A common complaint of classic PID controller design is that it is often
  > difficult to understand the effect of gain settings.

* It could very well be that the way we've applied classic PID controllers isn't
  suitable for unpredicatable internet traffic loads. There are branches of
  control theory might be better suited for this, see, for example,
  [robust](https://en.wikipedia.org/wiki/Robust_control) and
  [adaptive](https://en.wikipedia.org/wiki/Adaptive_control) control theory.
