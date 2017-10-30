accessing
nextPutAll: aCollection
	^ self isBinary
		ifTrue: [ super nextPutAll: aCollection ]
		ifFalse: [ aCollection do: [ :each | self nextPut: each ] ]