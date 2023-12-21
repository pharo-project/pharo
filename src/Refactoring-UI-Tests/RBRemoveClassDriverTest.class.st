Class {
	#name : 'RBRemoveClassDriverTest',
	#superclass : 'RBDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBRemoveClassDriverTest >> testBadPathRemovingNonEmptySubWithSubclasses [
	
	| pack driver |
	pack := RBPackageEnvironment packageNames: { 'Refactoring-UI-Tests' . 'Refactoring-UI' }.
	driver := RBRemoveClassDriver new 
		scopes: { pack }
		classes: { RBRemoveClassDriver }.
	self setUpDriver: driver.
	driver runRefactoring.
		
	self assert: driver breakingChoices size equals: 2
]

{ #category : 'tests' }
RBRemoveClassDriverTest >> testClassDoesNotExist [
	
	| pack driver |
	pack := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	driver := RBRemoveClassDriver new 
		scopes: { pack }
		classes: { MyClassC class}.
	self setUpDriver: driver.

	self shouldnt: [driver runRefactoring ] raise: RBRefactoringError.  
	
]

{ #category : 'tests' }
RBRemoveClassDriverTest >> testGoodPathClassIsUnusedEmptyNoSubclasses [
	
	| pack driver |
	pack := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	driver := RBRemoveClassDriver new 
		scopes: { pack }
		classes: { RBUnusedRootClass}.
	self setUpDriver: driver.
	driver runRefactoring.
	self assert: driver refactoring changes changes size equals: 1

]

{ #category : 'tests' }
RBRemoveClassDriverTest >> testGoodPathRemovingNonEmptyLeaf [
	
	| pack driver |
	pack := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	driver := RBRemoveClassDriver new 
		scopes: { pack }
		classes: { self class environment at: 'MyClassNonEmptyLeafUnused' asSymbol}.
	self setUpDriver: driver.
	driver runRefactoring.
	"It is not empty so we ask if the user really wants to lose something"
	self assert: driver breakingChoices size equals: 1

]
