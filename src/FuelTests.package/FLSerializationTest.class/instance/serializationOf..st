running
serializationOf: anObject 
	self streamFactory writeStreamDo: [:aStream |
	^ serializer serialize: anObject on: aStream. ].
