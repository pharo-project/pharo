tests
testEmpty
	| generator |
	generator := Generator on: [ :g | ].
	self assert: generator atEnd.
	self assert: generator peek isNil.
	self assert: generator next isNil