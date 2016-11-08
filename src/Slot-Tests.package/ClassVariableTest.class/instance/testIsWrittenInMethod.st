tests  - properties
testIsWrittenInMethod

	DefaultTimeLimit := DefaultTimeLimit. "writing class variable".
	
	self assert: ((TestCase classVariableNamed: #DefaultTimeLimit) isWrittenIn: self class >> testSelector)