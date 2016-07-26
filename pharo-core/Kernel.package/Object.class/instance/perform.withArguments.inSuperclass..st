message performing
perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."
	
	<reflective: #object:performMessageInSuperclass:>
	<primitive: 100>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self primitiveFailed