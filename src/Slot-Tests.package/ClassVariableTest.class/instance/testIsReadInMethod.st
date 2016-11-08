tests  - properties
testIsReadInMethod

	DefaultTimeLimit name. "reading class variable".
	
	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)