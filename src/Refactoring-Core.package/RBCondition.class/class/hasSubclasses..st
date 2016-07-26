instance creation
hasSubclasses: aClass
	^self new
		type: (Array with: #hasSubclasses with: aClass)
		block: [aClass subclasses isEmpty not]
		errorString: aClass printString , ' has <1?:no >subclasses'