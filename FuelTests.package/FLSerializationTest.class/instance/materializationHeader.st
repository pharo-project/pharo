running
materializationHeader
	
	self streamFactory readStreamDo: [:aStream |
		^ self materializer materializeHeaderFrom: aStream ]