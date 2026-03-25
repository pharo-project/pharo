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
StRefactoringPreviewPresenterMock class >> newApplication: anApplication [
	
	^ self new
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock class >> withChanges: changes scopes: aCollection [ 
	^ self new withChanges: changes scopes: aCollection 
]

{ #category : 'visiting' }
StRefactoringPreviewPresenterMock >> accept [


]

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock >> application: aStPharoApplication [ 

	application := aStPharoApplication
]

{ #category : 'initialization' }
StRefactoringPreviewPresenterMock >> changesFrom: aRefactoring scopes: aCollection [ 
	
	^ self withChanges: aRefactoring scopes: aCollection
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

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock >> withChanges: changeCol scopes: aCollection [ 
	changes := changeCol
]
