I am the combination of two RBEnvironments, a logical OR. That is: 
entity A is in this environment if it is in at least ONE environment I am constructed from.

Do not construct instances of me directly, use method #| for two existing environments:
env1 | env2 -> a RBOrEnvironment.