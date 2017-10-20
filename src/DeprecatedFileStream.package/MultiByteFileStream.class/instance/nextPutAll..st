accessing
nextPutAll: aCollection
	^ (self isBinary or: [ aCollection class == ByteArray ])
		ifTrue: [ super nextPutAll: aCollection ]
		ifFalse: [ self converter nextPutAll: aCollection toStream: self ]