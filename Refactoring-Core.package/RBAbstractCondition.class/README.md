I am the abstract base class of refactoring conditions.
Conditions are used by refactoring operation for checking preconditions before applying the 
refactoring.

I implement some common behavior for error handling - errorMacro.
And to combine conditions with boolean operatiosn (and/or/not).

Most of the condition checking behavior is implemented on my subclass 
RBCondition. 

Instances of RBConditions are created by factory methods on its class side.