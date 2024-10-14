Class {
	#name : 'ReExtractToTemporaryDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'initialization' }
ReExtractToTemporaryDriverTest >> setUpMocksOn: driver [

    | requestClass dialog |
    dialog := MockObject new.
    
    dialog 
        on: #variableName
        respond: ('temp').
    
    driver requestDialog: dialog.
    
    requestClass := MockObject new.
    requestClass
        on: #openOn:
        respond: dialog.
    
    driver variableNameEditorPresenterClass: requestClass.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testExtractToTemporaryExpectChanges [

	| driver driverChanges rbclasses|
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest }.
	driver := ReExtractToTemporaryDriver new
					scopes: { rbclasses };
					extract: (52 to: 73)
		         from: #caller2
		         in: RBClassDataForRefactoringTest.

	self setUpDriver: driver.
	
	self setUpMocksOn: driver.

	driver runRefactoring.
	driverChanges := driver refactoring changes.
	
	self assert: driverChanges changes size equals: 1.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testFailureInvalidVariableNameWhenVariableIsAlreadyTemporary [

	| driver rbclasses dialog requestClass|
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest }.
	driver := ReExtractToTemporaryDriver new
		scopes: { rbclasses };
		extract: (51 to: 60)
      from: #modifiesCollection
      in: RBClassDataForRefactoringTest.

   self setUpDriver: driver.

    dialog := MockObject new.
    dialog 
        on: #variableName
        respond: ('collection').
    driver requestDialog: dialog.
    requestClass := MockObject new.
    requestClass
        on: #openOn:
        respond: dialog.
    driver variableNameEditorPresenterClass: requestClass.

	self should: [ driver runRefactoring ] raise: RBRefactoringError.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testFailureNonExistantSelector [

	| driver rbclasses dialog requestClass |
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest }.
   driver := ReExtractToTemporaryDriver new
		scopes: { rbclasses };
		extract: (14 to: 23)
      from: #checkClass1:
      in: RBClassDataForRefactoringTest.
    
	self setUpDriver: driver.
	
	dialog := MockObject new.
    dialog 
        on: #variableName
        respond: ('asdf').
    driver requestDialog: dialog.
    requestClass := MockObject new.
    requestClass
        on: #openOn:
        respond: dialog.
    driver variableNameEditorPresenterClass: requestClass.
    
   self should: [ driver runRefactoring ] raise: RBRefactoringError.
]
