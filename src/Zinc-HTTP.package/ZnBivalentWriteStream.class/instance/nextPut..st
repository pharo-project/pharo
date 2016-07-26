accessing
nextPut: object
	self isBinary
		ifTrue: [ stream nextPut: object asInteger ]
		ifFalse: [ stream nextPut: object asCharacter ]