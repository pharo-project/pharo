Class {
	#name : 'RBRemoveClassDriverTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'initialization' }
RBRemoveClassDriverTest >> setUpDriver: driver [

	driver previewPresenterClass: StRefactoringPreviewPresenterMock.
	driver selectDialog: StSelectDialogMock new
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
