tests-streams
testWriteStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStream.
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	stream := reference writeStream.
	stream close