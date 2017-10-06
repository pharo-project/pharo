generating
generateOn: aStream
	aStream nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>'; nextPut: Character lf.
	aStream nextPutAll: '<report>'; nextPut: Character lf.
	self generateStatsOn: aStream.
	self generateDataOn: aStream.
	aStream nextPutAll: '</report>'; nextPut: Character lf