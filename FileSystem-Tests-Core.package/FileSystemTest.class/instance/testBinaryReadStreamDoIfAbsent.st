tests-streams-compatibility
testBinaryReadStreamDoIfAbsent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		binaryReadStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		binaryReadStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])