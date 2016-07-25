tests
testAssociations
	| associations |
	associations := self instance associations.
	self deny: associations isEmpty.
	self assert: associations anyOne key isString.
	self assert: associations anyOne value isString.