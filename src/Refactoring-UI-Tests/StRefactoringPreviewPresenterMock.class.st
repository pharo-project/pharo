Class {
	#name : 'StRefactoringPreviewPresenterMock',
	#superclass : 'Object',
	#instVars : [
		'changes',
		'application'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock class >> changes: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection [ 
	^ self new changes: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection 
]

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock >> application: aStPharoApplication [ 
	application := aStPharoApplication
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
