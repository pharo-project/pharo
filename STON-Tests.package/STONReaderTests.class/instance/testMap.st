tests
testMap
	self assert: (self materialize: '{#foo:1}') = (STON mapClass new at: #foo put: 1; yourself).
	self assert: (self materialize: '{}') = STON mapClass new