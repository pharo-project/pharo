Class {
	#name : 'StRefactoringPreviewPresenterMock',
	#superclass : 'Object',
	#instVars : [
		'changes'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock class >> changes: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection [ 
	^ self new changes: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection 
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock >> changes: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection [ 
	changes := aRBUpFrontPreconditionCheckingCompositeRefactoring
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> openModal [
	^ changes
]

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock >> refactoring: aRBUpFrontPreconditionCheckingCompositeRefactoring [ 
	"don't touch me this is for the mock"
]
