Class {
	#name : 'RBRenameInstanceVariableDriverTest',
	#superclass : 'RBDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRenameInstanceVariableDriverTest >> testInvalidName [
	
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
RBRenameInstanceVariableDriverTest >> testValidNameAndUnusedVariable [
	
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

{ #category : 'tests' }
RBRenameInstanceVariableDriverTest >> testValidNameAndUsedVariable [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameInstanceVariableDriver new
				scopes: { rbclasses }
				variable: #instVarName1
				for: MyClassARoot
				newName: #instVarName1a.
	self setUpDriver: driver.
	driver runRefactoring.
	"Renaming the references to the variable is not encapsulated in change objects, therefore the only change is the renaming of the variable itself."
	self assert: driver refactoring changes changes size equals: 1
	
]
