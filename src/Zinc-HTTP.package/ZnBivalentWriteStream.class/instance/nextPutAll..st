accessing
nextPutAll: collection
	self isBinary
		ifTrue: [ stream nextPutAll: collection asByteArray ]
		ifFalse: [ stream nextPutAll: collection asString ]