running
serializationOf: anObject includes: anotherObject 
	| serialization |
	self streamFactory writeStreamDo: [:aStream |
	serialization := serializer serialize: anObject on: aStream.
	^ serialization objects includes: anotherObject ].