identifier testing
isMethodArgName: aString 
	^ (arguments
		at: 0
		ifAbsent: [#()])
		includes: aString