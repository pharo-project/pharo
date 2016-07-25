tests-streams
testReadStreamDoIfAbsent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		readStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		readStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])