tests-streams
testWriteStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference writeStreamDo: [ :stream |
		stream nextPutAll: 'griffle'.
		true ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamDo: [ :stream | true ])