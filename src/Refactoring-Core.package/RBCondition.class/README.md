I am a refactoring condition for doing a precondition check for refactoring operations.

A precondition check returns true or false and is used by refactoring operations to check whether the operation is applicable to the target entity (class or method refactoring).

You can set the block code used for testing the condition (#withBlock:).
And I define some factory methods on my class side for creating instances of me, for some typically usage. 

For example: 
This creates a condition checking if the class named #Morph implements a selector named #drawOn:
(RBCondition definesSelector:#drawOn: in: (RBClass existingNamed:#Morph)).

Most users of me are refactoring operations and use my methods on the class side for creating instances.
