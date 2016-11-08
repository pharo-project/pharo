tests  - properties
testNotReadInMethod

	self deny: ((TestCase classVariableNamed: #DefaultTimeLimit) isReadIn: self class >> testSelector)