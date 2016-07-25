printing
printOn: aStream 
	| string |
	aStream nextPutAll: '3PButton'.
	arguments notEmpty 
		ifTrue: [string := arguments at: (2 min: arguments size)].
	aStream nextPutAll: '('.
	(string notNil and: [string ~~ self]) 
		ifTrue: 
			[aStream
				print: string;
				space]
		ifFalse: 
			[aStream
				print: actionSelector;
				space].
	aStream
		print: self identityHash;
		nextPutAll: ')'