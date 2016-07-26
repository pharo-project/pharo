tests
testModelAbstractClassVariableOverridenMethodsInSubclass
	| refactoring meta class |
	class := model classNamed: #Foo.
	meta := class theMetaClass.
	refactoring := RBAbstractClassVariableRefactoring 
		model: model
		variable: 'ClassVarName2'
		class: class.
	self executeRefactoring: refactoring.
	self assert: (meta parseTreeFor: #classVarName21) = (RBParser parseMethod: 'classVarName21 ^ClassVarName2').
	self assert: (meta parseTreeFor: #classVarName21:) = (RBParser parseMethod: 'classVarName21: anObject ClassVarName2 := anObject')