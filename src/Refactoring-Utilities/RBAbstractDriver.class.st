Class {
	#name : #RBAbstractDriver,
	#superclass : #Object,
	#instVars : [
		'context'
	],
	#category : #'Refactoring-Utilities'
}

{ #category : #'instance creation' }
RBAbstractDriver class >> context: aToolContext [
	^ self new
		context: aToolContext;
		yourself
]

{ #category : #actions }
RBAbstractDriver >> applyResult [
]

{ #category : #creating }
RBAbstractDriver >> asRefactorings [
	self subclassResponsibility
]

{ #category : #actions }
RBAbstractDriver >> confirmRefactoringInContext [
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
RBAbstractDriver >> context: aToolContext [
	context := aToolContext
]

{ #category : #actions }
RBAbstractDriver >> executeRefactoring [ 
	|refactoring|
	self prepareFullExecution.
	refactoring := self asRefactorings.
	refactoring do: [ :each | each execute ]
]

{ #category : #testing }
RBAbstractDriver >> isComplexRefactoring [ 
	^ false
]

{ #category : #actions }
RBAbstractDriver >> prepareFullExecution [
	self subclassResponsibility
]
