stream-protocol
at: index put: anObject
	index > bytes size
		 ifTrue: [ self grow ].
		
	bytes at: index put: (anObject isCharacter
		ifTrue: [ anObject codePoint ]
		ifFalse: [ anObject ]).
	
	size := size max: index