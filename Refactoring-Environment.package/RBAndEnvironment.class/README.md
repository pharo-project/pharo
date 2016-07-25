I am the combination of two RBEnvironments, a logical AND. That is: 
entity A is in this environment if it is in BOTH environment I am constructed from.

Do not construct instances of me directly, use method #& for two existing environments:
env1 & env2 -> a RBAndEnvironment.