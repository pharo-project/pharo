I am the abstract base class for refactoring operations. 

I define the common workflow for a refactoring:
check precondition, 
primitive execute - a dry run collecting the changes without applying them,
and execute - run and apply changes.

I provide many utility methods used by my subclasses. 
Every  concrete subclass implements a single refactoring. They have to implement the methods
preconditions and transform.


Instance Variables

options:
Some refactorings may need user interactions or some extra data for performing
the operation, the code for requesting this data is stored in a block associated with a "refacotring option"
(see RBRefactoring>>#setOption:toUse:  and RBRefactoring class>>#initializeRefactoringOptions).

model:
My model - a RBNamespace - defines the environment in which my refactoring is applied and collects all changes (RBRefactoryChange).

A RBRefactoringManager  is used to collect the executed refactorings and provides an undo and redo facility.
