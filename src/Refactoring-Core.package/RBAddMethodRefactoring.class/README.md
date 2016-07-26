I am a refactoring for adding methods to a class.

My operation will compile a new method to a class in the specified protocol.

You can create an instance with: 
RBAddMethodRefactoring model:RBNamespace new addMethod:'foo ^ self' toClass:Morph inProtocols:{'test'}.

The method to compile is the full method source (selector, arguments and code).

My precondition verifies that the methods source can be parsed and that the class does not already understands this methods selectors. That means, you can not use this refactoring to add methods for overwriting superclass methods.
