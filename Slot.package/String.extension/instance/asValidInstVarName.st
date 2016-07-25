*Slot
asValidInstVarName

	| validName |
	
	validName := self select: [ :c | c isAlphaNumeric or: [ c == $_ ] ].
	
	^validName first isDigit 
		ifTrue: [ 'v' , validName ]
		ifFalse: [ validName ]