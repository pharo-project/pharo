tests
testAbstractWithAssignmentUsed
	| refactoring class |
	class := model classNamed: #Foo.
	refactoring := RBAbstractInstanceVariableRefactoring 
				model: model
				variable: 'instVarName2'
				class: class.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo) 
				= (RBParser parseMethod: 'foo
						^self instVarName21: 3').
	self 
		assert: (class parseTreeFor: #instVarName2:) = (RBParser 
						parseMethod: 'instVarName2: anObject
						self instVarName21: anObject').
	self 
		assert: (class parseTreeFor: #instVarName21:) = (RBParser 
						parseMethod: 'instVarName21: anObject
						^instVarName2 := anObject').
	self assert: (class parseTreeFor: #instVarName2) 
				= (RBParser parseMethod: 'instVarName2
						^instVarName2').
	self 
		assert: ((model classNamed: #Bar) parseTreeFor: #foo) = (RBParser 
						parseMethod: 'foo
						instVarName1 := instVarName1 + self instVarName2 + ClassVarName1')