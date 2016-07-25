running
serialize: anObject

	self streamFactory writeStreamDo: [:aStream |
		self serializer serialize: anObject on: aStream ]