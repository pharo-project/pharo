instance creation
isImmediateSubclass: subclass of: superClass
	^self new
		type: (Array with: #immediateSubclass with: superClass with: subclass)
		block: [subclass superclass = superClass]
		errorString: subclass printString , ' is <1?:not >an immediate subclass of ' , superClass printString