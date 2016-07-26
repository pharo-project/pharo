filter streaming
putOn: aStream
	aStream isBinary ifFalse: [ 
		self asString putOn: aStream.
		^ self ].
	self asByteArray do: [ :each | aStream nextPut: each ]