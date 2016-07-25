I am an abstract superclass for objects that fulfill the Guide role in the Guide/Visitor pattern. My subclasses know how to traverse a filesystem in a specific order, "showing" the files and directories they encounter to a visitor.

visitor
	An object that fulfills the Visitor role and implements the visitor protocol.
	
work
	An OrderedCollection, used to keep track of filesystem nodes that have not yet been visited