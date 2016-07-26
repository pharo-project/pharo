tests-streams-compatibility
testBinaryReadStreamIfAbsent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference binaryReadStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close