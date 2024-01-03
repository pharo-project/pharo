Class {
	#name : 'ReRemoveClassDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReRemoveClassDriverTest >> testBadPathRemovingNonEmptySubWithSubclasses [
	
	| pack driver |
	pack := RBPackageEnvironment packageNames: { 'Refactoring-UI-Tests' . 'Refactoring-UI' }.
	driver := ReRemoveClassDriver new 
		scopes: { pack }
		classes: { ReRemoveClassDriver }.
	self setUpDriver: driver.
	driver runRefactoring.
		
	self assert: driver breakingChoices size equals: 2
]

{ #category : 'tests' }
ReRemoveClassDriverTest >> testClassDoesNotExist [
	
	| pack driver |
	pack := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	driver := ReRemoveClassDriver new 
		scopes: { pack }
		classes: { MyClassC class}.
	self setUpDriver: driver.

	self shouldnt: [driver runRefactoring ] raise: RBRefactoringError.  
	
]

{ #category : 'tests' }
ReRemoveClassDriverTest >> testGoodPathClassIsUnusedEmptyNoSubclasses [
	
	| pack driver |
	pack := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	driver := ReRemoveClassDriver new 
		scopes: { pack }
		classes: { RBUnusedRootClass}.
	self setUpDriver: driver.
	driver runRefactoring.
	self assert: driver refactoring changes changes size equals: 1

]

{ #category : 'tests' }
ReRemoveClassDriverTest >> testGoodPathRemovingNonEmptyLeaf [
	
	| pack driver |
	pack := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	driver := ReRemoveClassDriver new 
		scopes: { pack }
		classes: { self class environment at: 'MyClassNonEmptyLeafUnused' asSymbol}.
	self setUpDriver: driver.
	driver runRefactoring.
	"It is not empty so we ask if the user really wants to lose something"
	self assert: driver breakingChoices size equals: 1

]
