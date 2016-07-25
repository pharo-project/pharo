compiling
change: aSelector toReturn: anObject
	self 
		compileClass: self mockClassA 
		source: aSelector, ' ^ ', anObject printString 
		category: 'numeric'