Class {
	#name : 'ReExtractToTemporaryDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'driver'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testExtractToTemporaryExpectChanges [

	| driverChanges |
	driver := ReExtractToTemporaryDriver new
					 extract: (130 to: 138)
					 to: 'variable'
		          from: #noMoveDefinition
		          in: RBClassDataForRefactoringTest.

	self setUpDriver: driver.

	driver runRefactoring.
	driverChanges := driver refactoring changes.
	
	self assert: driverChanges changes size equals: 1.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testFailureInvalidVariableName [

    driver := ReExtractToTemporaryDriver new
        extract: (51 to: 60)
        to: 'collection'
        from: #modifiesCollection
        in: RBClassDataForRefactoringTest.

    self setUpDriver: driver.
    
    self should: [ driver runRefactoring ] raise: Error.
]

{ #category : 'tests' }
ReExtractToTemporaryDriverTest >> testFailureNonExistantSelector [
    
    driver := ReExtractToTemporaryDriver new
        extract: (14 to: 23)
        to: 'asdf'
        from: #checkClass1:
        in: RBClassDataForRefactoringTest.
    
    self setUpDriver: driver.
    
    self should: [ driver runRefactoring ] raise: Error.
]
