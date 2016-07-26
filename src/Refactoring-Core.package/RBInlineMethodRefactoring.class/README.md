I am a refactoring for replacing method calls by the method implementation.

You can select a message send in a method and refactoring this message send to inline its code.
Any temporary variable used in the original message send are placed into this method and renamed if there are already variables with this name.

My preconditions verify that the inlined method is not a primitive call, the method does not have multiple returns and shows a warning if the method is overriden in subclasses.

