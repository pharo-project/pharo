Class {
	#name : 'RBRenameSharedVariableDriverTest',
	#superclass : 'RBDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testInvalidName [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameSharedVariableDriver new
				scopes: { rbclasses }
				variable: #Shared1
				for: MyClassARoot
				newName: #Shared2.
	"There is already a variable named `#Shared2` in the class `MyClassRoot`"
	self setUpDriver: driver.
	
	self should: [driver runRefactoring] raise: RBRefactoringError
	
]

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testValidNameAndUndefinedVariable [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameSharedVariableDriver new
				scopes: { rbclasses }
				variable: #Shared3
				for: MyClassARoot
				newName: #Shared3a.
	"Variable `Shared3` is not defined in `MyClassARoot`"
	self setUpDriver: driver.
	
	self should: [driver runRefactoring] raise: RBRefactoringError
	
]

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testValidNameAndUnusedVariable [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameSharedVariableDriver new
				scopes: { rbclasses }
				variable: #Shared2
				for: MyClassARoot
				newName: #Shared2a.
	self setUpDriver: driver.
	driver runRefactoring.
	
	self assert: driver refactoring changes changes size equals: 1
	
]

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testValidNameAndUsedVariable [
	
	| rbclasses driver |
	rbclasses := RBClassEnvironment classes: { MyClassARoot}.	
	driver := RBRenameSharedVariableDriver new
				scopes: { rbclasses }
				variable: #Shared2
				for: MyClassARoot
				newName: #Shared2a.
	self setUpDriver: driver.
	driver runRefactoring.
	"Renaming the references to the variable is not encapsulated in change objects, therefore the only change is the renaming of the variable itself."
	self assert: driver refactoring changes changes size equals: 1
	
]
