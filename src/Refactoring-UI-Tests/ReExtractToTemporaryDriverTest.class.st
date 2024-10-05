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
        respond: (RBArgumentName name: 'tempVar').
    
    driver requestDialog: dialog.
    
    requestClass := MockObject new.
    requestClass
        on: #openOn:withInvalidArgs:canRenameArgs:canRemoveArgs:canAddArgs:canEditName:
        respond: dialog.
    
    driver variableNameEditorPresenterClass: requestClass.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testExtractToTemporaryExpectChanges [

	| driver driverChanges |
	driver := ReExtractToTemporaryDriver new
					extract: (130 to: 138)
					to: 'variable'
		         from: #noMoveDefinition
		         in: RBClassDataForRefactoringTest.

	self setUpDriver: driver.
	
	self setUpMocksOn: driver.

	driver runRefactoring.
	driverChanges := driver refactoring changes.
	
	self assert: driverChanges changes size equals: 1.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testFailureInvalidVariableName [

	| driver |
	driver := ReExtractToTemporaryDriver new
		extract: (51 to: 60)
      to: 'collection'
      from: #modifiesCollection
      in: RBClassDataForRefactoringTest.

   self setUpDriver: driver.

	self setUpMocksOn: driver.

	self should: [ driver runRefactoring ] raise: RBRefactoringError.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testFailureNonExistantSelector [

	| driver |    
   driver := ReExtractToTemporaryDriver new
		extract: (14 to: 23)
      to: 'asdf'
      from: #checkClass1:
      in: RBClassDataForRefactoringTest.
    
	self setUpDriver: driver.
    
   self should: [ driver runRefactoring ] raise: RBRefactoringError.
]
