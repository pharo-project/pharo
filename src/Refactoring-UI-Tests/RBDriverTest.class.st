Class {
	#name : 'RBDriverTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'initialization' }
RBDriverTest >> setUpDriver: driver [

	driver previewPresenterClass: StRefactoringPreviewPresenterMock.
	driver selectDialog: StSelectDialogMock new
]
