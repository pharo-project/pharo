Class {
	#name : 'RBRenameInstanceVariableDriverTest',
	#superclass : 'RBDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRenameInstanceVariableDriverTest >> testGoodPathAndInvalidName [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameInstanceVariableDriver new
				scopes: { rbclasses }
				variable: #unusedVar1
				for: MyClassARoot
				newName: #unusedVar2.
	"There is already a variable named `unusedVar2` in the class `MyClassRoot`"
	self setUpDriver: driver.
	
	self should: [driver runRefactoring] raise: RBRefactoringError
	
]

{ #category : 'tests' }
RBRenameInstanceVariableDriverTest >> testGoodPathAndValidName [
	
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
