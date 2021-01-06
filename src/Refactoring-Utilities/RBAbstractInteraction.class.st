Class {
	#name : #RBAbstractInteraction,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'Refactoring-Utilities'
}

{ #category : #'instance creation' }
RBAbstractInteraction class >> context: aToolContext [
	^ self new
		context: aToolContext;
		yourself
]

{ #category : #actions }
RBAbstractInteraction >> applyResult [
]

{ #category : #creating }
RBAbstractInteraction >> asRefactorings [
	self subclassResponsibility
]

{ #category : #actions }
RBAbstractInteraction >> confirmRefactoringInContext [
	| submitDialog |
	submitDialog := SycRefactoringPreviewPresenter
		for: self
		scopes: context refactoringScopes.
	submitDialog existChanges ifTrue:[
	submitDialog openModalWithSpec
		okAction: [ submitDialog accept.
			self applyResult ];
		cancelAction: [ submitDialog cancel ]]
	ifFalse: [ submitDialog alert: 'There is no change produced by this refactoring' ].
	CmdCommandAborted signal
]

{ #category : #accessing }
RBAbstractInteraction >> context: aToolContext [
	context := aToolContext
]

{ #category : #actions }
RBAbstractInteraction >> executeRefactoring [ 
	|refactoring|
	self prepareFullExecution.
	refactoring := self asRefactorings.
	refactoring do: [ :each | each execute ]
]

{ #category : #testing }
RBAbstractInteraction >> isComplexRefactoring [ 
	^ false
]

{ #category : #actions }
RBAbstractInteraction >> prepareFullExecution [
	self subclassResponsibility
]
