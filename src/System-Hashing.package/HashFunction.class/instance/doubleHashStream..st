accessing
doubleHashStream: aStream
	^ self hashStream: ((self hashStream: aStream) asByteArray readStream)