instance creation
isSubclass: subclass of: superClass
	^self new
		type: (Array with: #subclass with: superClass with: subclass)
		block: [subclass includesClass: superClass]
		errorString: subclass printString , ' is <1?:not >a subclass of ' , superClass printString