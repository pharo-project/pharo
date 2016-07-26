tests-streams
testReadingAfterWriting
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self deny: reference exists.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents equals: 'griffle'.
	stream close