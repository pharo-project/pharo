tests
testInstVarNamed
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj variable: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' ] raise: InstanceVariableNotFound.
	self should: [ obj instVarNamed: 'timoleon' ] raise: InstanceVariableNotFound.
	self
		assert:
			([ obj instVarNamed: 'timoleon' ]
				on: InstanceVariableNotFound
				do: [ :ex | ex instVarName ])
		equals: 'timoleon'