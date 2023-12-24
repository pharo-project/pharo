Class {
	#name : 'RBRenameSharedVariableDriverTest',
	#superclass : 'RBDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testInvalidNameFollowedByAValideOne [

	| rbclasses driver requestDialog |
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	driver := RBRenameSharedVariableDriver new
		          scopes: { rbclasses }
		          variable: #Shared1
		          for: MyClassARoot.
	"There is already a variable named `#Shared2` in the class `MyClassRoot`"
	self setUpDriver: driver.
	requestDialog := MockObject new.
	requestDialog on: #openModal respond: #Shared2.
	requestDialog on: #openModal respond: #Shared277676.
	requestDialog on: #title: respond: 'nothing'.
	requestDialog on: #text: respond: 'nothing'.
	driver requestDialog: requestDialog.
	driver runRefactoring
]

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testValidNameAndUndefinedVariable [

	| rbclasses driver requestDialog |
	self skip.
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	driver := RBRenameSharedVariableDriver new
		          scopes: { rbclasses }
		          variable: #Shared3
		          for: MyClassARoot.
	"Variable `Shared3` is not defined in `MyClassARoot`"
	self setUpDriver: driver.
	requestDialog := MockObject new.
	requestDialog on: #openModal respond: #Shared2.
	requestDialog on: #openModal respond: #Shared277676.
	requestDialog on: #title: respond: 'nothing'.
	requestDialog on: #text: respond: 'nothing'.
	driver requestDialog: requestDialog.

	"self halt."
	"It loops from here"
	driver runRefactoring
]

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testValidNameAndUnusedVariable [

	| rbclasses driver requestDialog |
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	driver := RBRenameSharedVariableDriver new
		          scopes: { rbclasses }
		          variable: #Shared2
		          for: MyClassARoot.
	self setUpDriver: driver.
	requestDialog := MockObject new.
	requestDialog on: #openModal respond: #Shared277676.
	requestDialog on: #title: respond: 'nothing'.
	requestDialog on: #text: respond: 'nothing'.
	driver requestDialog: requestDialog.
	
	driver runRefactoring.
	
	self assert: driver refactoring changes changes size equals: 1
]

{ #category : 'tests' }
RBRenameSharedVariableDriverTest >> testValidNameAndUsedVariable [

	| rbclasses driver requestDialog |
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	driver := RBRenameSharedVariableDriver new
		          scopes: { rbclasses }
		          variable: #Shared2
		          for: MyClassARoot.
	self setUpDriver: driver.

	"Renaming the references to the variable is not encapsulated in change objects, 
	therefore the only change is the renaming of the variable itself."
	
	requestDialog := MockObject new.
	requestDialog on: #openModal respond: #Shared2a.
	requestDialog on: #title: respond: 'nothing'.
	requestDialog on: #text: respond: 'nothing'.
	driver requestDialog: requestDialog.
	
	driver runRefactoring.
	self assert: driver refactoring changes changes size equals: 1
]
