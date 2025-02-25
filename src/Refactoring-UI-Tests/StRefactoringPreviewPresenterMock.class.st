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
StRefactoringPreviewPresenterMock class >> for: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection [ 
	^ self new for: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection 
]

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock >> application: aStPharoApplication [ 
	application := aStPharoApplication
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock >> for: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection [ 
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
