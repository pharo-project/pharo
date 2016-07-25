tests-streams-compatibility
testBinaryReadStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference binaryReadStream ] raise: FileDoesNotExist.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close