tests
testInstVarNamed
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj variable: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' ] raise: NoInstanceVariableNamed.
	self should: [ obj instVarNamed: 'timoleon' ] raise: NoInstanceVariableNamed.
	self
		assert:
			([ obj instVarNamed: 'timoleon' ]
				on: NoInstanceVariableNamed
				do: [ :ex | ex instVarName ])
		equals: 'timoleon'