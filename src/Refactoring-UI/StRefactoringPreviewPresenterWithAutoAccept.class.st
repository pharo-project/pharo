"
Instead displaying the changes and apply only those selected by the user, I automatically apply all my changes at the moment I receive the message >>openModal
"
Class {
	#name : 'StRefactoringPreviewPresenterWithAutoAccept',
	#superclass : 'StRefactoringPreviewPresenter',
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'showing' }
StRefactoringPreviewPresenterWithAutoAccept >> openModal [
	self performChanges
]
