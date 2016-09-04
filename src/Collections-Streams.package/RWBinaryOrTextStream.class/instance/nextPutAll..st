accessing
nextPutAll: aCollection
	"Optimized for ByteArrays"
	^ aCollection class == ByteArray 
		ifTrue: [ super nextPutAll: aCollection asString ]
		ifFalse: [ super nextPutAll: aCollection ]