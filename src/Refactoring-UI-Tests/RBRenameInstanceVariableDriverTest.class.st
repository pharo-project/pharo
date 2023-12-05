Class {
	#name : 'RBRenameInstanceVariableDriverTest',
	#superclass : 'RBDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRenameInstanceVariableDriverTest >> testGoodPathVariableIsUnused [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameInstanceVariableDriver new
				scopes: { rbclasses }
				variable: #unusedVar1
				for: MyClassARoot
				newName: #unusedVar1a.
	self setUpDriver: driver.
	driver runRefactoring.
	
	self assert: driver refactoring changes changes size equals: 1
	
]
