tests  - properties
testIsReadInMethod

	DefaultTimeLimit printString. "reading class variable".
	
	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)