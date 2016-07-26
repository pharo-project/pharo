tests
testModelAbstractClassVariable
	| refactoring meta class |
	class := model classNamed: #Foo.
	meta := class theMetaClass.
	refactoring := RBAbstractClassVariableRefactoring 
		model: model
		variable: 'ClassVarName1'
		class: class.
	self executeRefactoring: refactoring.
	self assert: (meta parseTreeFor: #classVarName1) = (RBParser parseMethod: 'classVarName1 ^ClassVarName1').
	self assert: (meta parseTreeFor: #classVarName1:) = (RBParser parseMethod: 'classVarName1: anObject ^ClassVarName1 := anObject').
	self assert: (meta parseTreeFor: #foo) = (RBParser parseMethod: 'foo
					^self classVarName1: self classVarName1 * self classVarName1 * self classVarName1').
	self assert: (class parseTreeFor: #classVarName1) = (RBParser parseMethod: 'classVarName1
							^self class classVarName1').
	self assert: (class parseTreeFor: #classVarName1:) = (RBParser parseMethod: 'classVarName1: anObject
							^self class classVarName1: anObject').
	self assert: (class parseTreeFor: #asdf) = (RBParser parseMethod: 'asdf
						^self classVarName1: (self class classVarName1: self class classVarName1 + 1)').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo) = (RBParser parseMethod: 'foo
					instVarName1 := instVarName1 + instVarName2 + self class classVarName1').
	self assert: ((model classNamed: #Bar) parseTreeFor: #foo) = (RBParser parseMethod: 'foo
						instVarName1 := instVarName1 + instVarName2 + self class classVarName1')