tests
testAbstractWithDefaultNamesUsed
	| refactoring class |
	class := model classNamed: #Foo.
	refactoring := RBAbstractInstanceVariableRefactoring 
				model: model
				variable: 'instVarName1'
				class: class.
	self executeRefactoring: refactoring.
	self 
		assert: (class parseTreeFor: #bar) = (RBParser 
						parseMethod: 'bar
						"Add one to instVarName1"

						self instVarName11: self instVarName11 + 1').
	self 
		assert: (class parseTreeFor: #instVarName11:) = (RBParser 
						parseMethod: 'instVarName11: anObject
						instVarName1 := anObject').
	self assert: (class parseTreeFor: #instVarName11) 
				= (RBParser parseMethod: 'instVarName11
						^instVarName1').
	self 
		assert: ((model classNamed: #Bar) parseTreeFor: #foo) = (RBParser 
						parseMethod: 'foo
						self instVarName11: self instVarName11 + instVarName2 + ClassVarName1')