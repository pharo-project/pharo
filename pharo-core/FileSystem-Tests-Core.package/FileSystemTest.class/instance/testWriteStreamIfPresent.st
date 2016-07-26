tests-streams
testWriteStreamIfPresent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStreamIfPresent: [ false ].
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamIfPresent: [ true ])