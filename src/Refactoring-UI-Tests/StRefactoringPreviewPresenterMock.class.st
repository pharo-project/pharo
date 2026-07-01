Class {
	#name : 'StRefactoringPreviewPresenterMock',
	#superclass : 'StRefactoringPreviewPresenter2',
	#instVars : [
		'lastInstance',
		'openModal',
		'opened',
		'performed',
		'updated',
		'closed'
	],
	#classInstVars : [
		'lastInstance'
	],
	#category : 'Refactoring-UI-Tests-Mocks',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Mocks'
}

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock class >> lastInstance [
    ^ lastInstance 
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock class >> newApplication: anAppliccation [
	lastInstance := self new.
	^lastInstance 
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock class >> withChanges: changes scopes: aCollection [ 
	^ self new withChanges: changes scopes: aCollection 
]

{ #category : 'visiting' }
StRefactoringPreviewPresenterMock >> accept [

    (self okToChange not or: [ selectedChanges isEmptyOrNil ]) ifTrue: [
        self closeWindow.
        ^ self ].

    self performChanges.
    self updateGlobalActiveScope.
    self closeWindow.
]

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock >> application: aStPharoApplication [ 

	application := aStPharoApplication
]

{ #category : 'initialization' }
StRefactoringPreviewPresenterMock >> changesFrom: aRefactoring scopes: aCollection [ 
	
	^ self withChanges: aRefactoring scopes: aCollection
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> closeWindow [
    closed := true.
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> closed [ 
	^ closed 
	
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> driver [
	^ driver 
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> driver: aDriver [
    driver := aDriver.
    ^ self
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock >> for: aRBUpFrontPreconditionCheckingCompositeRefactoring scopes: aCollection [ 
	changes := aRBUpFrontPreconditionCheckingCompositeRefactoring
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> lastInstance: anObject [

	lastInstance := anObject
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> name [
	^ name
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> name: aString [
    name := aString.
    ^ self
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> openModal [
    opened := true
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> opened [
	^opened 
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> performChanges [
    performed := true.
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> performed [
	^ performed 
]

{ #category : 'accessing' }
StRefactoringPreviewPresenterMock >> refactoring: aRBUpFrontPreconditionCheckingCompositeRefactoring [ 
	"don't touch me this is for the mock"
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> scopes [
	^scopes 
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> scopes: someScopes [
    scopes := someScopes
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> updateGlobalActiveScope [
    updated := true.
]

{ #category : 'api - showing' }
StRefactoringPreviewPresenterMock >> updated [ 
	^ updated 
]

{ #category : 'instance creation' }
StRefactoringPreviewPresenterMock >> withChanges: changeCol scopes: aCollection [ 
	changes := changeCol
]
