instance creation
fromMethodTimeStamp: aString
	| stream |
	stream := aString readStream.
	stream
		skipSeparators;
		skipTo: Character space.
	^ self readSeparateDateAndTimeFrom: stream