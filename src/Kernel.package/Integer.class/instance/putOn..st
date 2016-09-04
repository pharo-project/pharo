streaming
putOn: aStream
	aStream isBinary
		ifTrue: [ self asByteArray do: [ :each | aStream nextPut: each ] ]
		ifFalse: [ self asString putOn: aStream ]