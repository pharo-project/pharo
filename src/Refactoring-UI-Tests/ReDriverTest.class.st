Class {
	#name : 'ReDriverTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'initialization' }
ReDriverTest >> setUpDriver: driver [

	driver previewPresenterClass: StRefactoringPreviewPresenterMock.
	driver selectDialogForBreakingChanges: StSelectDialogMock new.
	driver informDialog: StInformDialogMock new.
]
