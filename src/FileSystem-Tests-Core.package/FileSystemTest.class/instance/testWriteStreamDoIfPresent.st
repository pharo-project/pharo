tests-streams
testWriteStreamDoIfPresent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		writeStreamDo: [ :stream |
			stream nextPutAll: 'griffle'.
			true ]
		ifPresent: [ false ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference 
		writeStreamDo: [ :stream | true ]
		ifPresent: [ true ])