message performing
withReceiver: receiver perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses"
	<primitive: 100 error: error>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = self indexableSizeOf: argArray)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	((self classOf: receiver) == lookupClass or: [(self classOf: receiver) inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self primitiveFailed