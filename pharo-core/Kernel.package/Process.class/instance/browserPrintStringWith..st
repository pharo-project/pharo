printing
browserPrintStringWith: anObject 
	| stream |
	stream := (String new: 100) writeStream.
	stream nextPut: $(.
	priority printOn: stream.
	self isSuspended
		ifTrue: [stream nextPut: $s].
	stream nextPutAll: ') '.
	stream nextPutAll: self name.
	stream nextPut: $:.
	stream space.
	stream nextPutAll: anObject asString.
	^ stream contents