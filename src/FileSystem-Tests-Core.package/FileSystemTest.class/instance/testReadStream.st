tests-streams
testReadStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference readStream ] raise: FileDoesNotExist.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close