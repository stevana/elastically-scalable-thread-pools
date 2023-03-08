# Elastically scalable thread pools

*PLEASE DON'T SHARE JUST YET, WRITE UP STILL IN PROGRESS!*

An experiment in controlling the size of a thread pool using a PID controller.

## Motivation

* Split work up in stages, pipelining
* If one stage is slow, throw an extra thread on it
* Spin up more threads
  - decrease latency (waiting time in the queue)
  - increase throughput (more enqueued items processed)
* Spin down threads
  - let some other stage use the CPU resources
  - save energy/money

## Plan

* One stage of a pipeline

```
   --->[In queue]--->[Worker pool]--->[Out queue]--->
```

* if in queue grows/shrinks, increase/decrease amount of workers in the pool

* use PID controller to stabilise amount of workers even though work fluctuates

```
                                            +----------------------------------+
                                            |                                  |
    -------------------------------------------->[In queue]--->[Worker pool]------->[Out queue]--->
                                            |                                  |
     r(t)   e(t)                    u(t)    |                                  |
    ----->+------>[PID controller]--------> |                                  |
          ^                                 |                                  |
          |                                 +----------------------------------+
          |                                                 | y(t)
          +-------------------------------------------------+

```

## Code

### Main
### Pool
### Load generator
### PID controller
### Monitor

## How it works

![](img/elastically-scalable-thread-pools-1.0-0.0-0.0.svg){ width= 400px }

## Contributing

* Determinism, shards (odd/even, mod N) instead of pools a la Disruptor
  - orthonogal
  - but makes testing easier

* Larger pipelines, is it enough to

* Controlling other things, e.g. batch size?

* Is it robust to wildly changing usage patterns?
  - [Slashdot effect](https://en.wikipedia.org/wiki/Slashdot_effect)
  - Bursty traffic

* We've looked at scaling up and down on a single machine (vertical scaling),
  what about scaling out and in across multiple machines (horizontal scaling)?

## See also

* https://www.researchgate.net/publication/265611546_A_Review_of_Auto-scaling_Techniques_for_Elastic_Applications_in_Cloud_Environments
  - benchmark suites
  - 709 citations

  - https://www.youtube.com/watch?v=BOKqGPWXwk0
  - [SEDA: An Architecture for Well-Conditioned Scalable Internet
    Services](https://people.eecs.berkeley.edu/~brewer/papers/SEDA-sosp.pdf)
  - https://people.eecs.berkeley.edu/~brewer/papers/SEDA-sosp.pdf

* [PID Controller Implementation in
  Software](https://youtube.com/watch?v=zOByx3Izf5U)

* [Control theory](https://en.wikipedia.org/wiki/Control_theory)
  - [Robust Control Theory](https://users.ece.cmu.edu/~koopman/des_s99/control_theory/)
  - [PID tuning](https://en.wikipedia.org/wiki/PID_controller#Loop_tuning)
