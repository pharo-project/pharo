identifier testing
isMethodTempName: aString 
	((arguments
			at: 0
			ifAbsent: [#()])
			includes: aString)
		ifTrue: [^ false].
	^ (temporaries
		at: 0
		ifAbsent: [#()])
		includes: aString