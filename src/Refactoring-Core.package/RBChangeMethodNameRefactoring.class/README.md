I am an abstract base class for refactorings changing a method name.

Doing a method rename involves:
rename implementors
rename message sends and
remove renamed implementors.

I implement the above precedures and provide helper functions for finding and renaming references.
Every concrete subclass has to add its own precondition (see #myPrecondition).
