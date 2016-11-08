tests  - properties
testNotWrittenInMethodWhenItIsOnlyRead

	DefaultTimeLimit printString. "reading class variable".
	
	self deny: ((TestCase classVariableNamed: #DefaultTimeLimit) isWrittenIn: self class >> testSelector)