ui
gtDisplayOn: aStream
	self printOn: aStream.
	aStream nextPutAll: ' ('.
	self gtDisplaySummaryOn: aStream.
	aStream nextPutAll: ')'