tests
testInstVarNamedPut
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj instVarNamed: 'variable' put: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' put: 1 ] raise: InstanceVariableNotFound.
	self should: [ obj instVarNamed: 'timoleon' put: 1 ] raise: InstanceVariableNotFound