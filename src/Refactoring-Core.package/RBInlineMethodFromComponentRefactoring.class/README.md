I am a refactoring for replacing method calls by the method implementation.

Just like RBInlineMethodRefactoring,  I replace a message send by the implementation of that  message , but you can provide the component
where this implementation is taken from or choose one if there are move than one implementors.
If the method implementation has some direct variable references, accessor for this variable are created (just as by the generate accessor refactoring).