converting
checkBufferSpace: minimalSize
	| size |
	
	size := buffer ifNil: [ 0 ] ifNotNil: [ buffer size ].
	
	size < minimalSize ifTrue: [
		buffer := ByteArray new: minimalSize.
		bs := buffer writeStream.
		
		]
