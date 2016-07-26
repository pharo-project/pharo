tests-streams-compatibility
testBinaryReadStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference binaryReadStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExist.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'