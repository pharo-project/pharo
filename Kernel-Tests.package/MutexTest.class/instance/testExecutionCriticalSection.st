tests
testExecutionCriticalSection

	| actual |
	actual := mutex critical: [ #result ].
	
	self assert: actual equals: #result