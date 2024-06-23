"
Managing the refactoring of duplicate classes within the codebase. Duplicate classes can lead to code redundancy, maintenance challenges, and potential inconsistencies. 

# Responsibilities

- Configure the duplicate class refactoring.
- Provide a dialog to write a new class name.
- Provide a selection list of methods to be copied or not..
- Ensure that the refactoring process maintains the integrity and functionality of the original classes.

"
Class {
	#name : 'ReDuplicateClassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'className',
		'newClassName',
		'rbClass'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReDuplicateClassDriver class >> className: aClassName [

	^ self new
		className: aClassName;
		yourself
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> className: aClassName [

	className := aClassName
]

{ #category : 'execution' }
ReDuplicateClassDriver >> configureRefactoring [

	refactoring := ReCopyClassRefactoring new
		copyClass: self className
		withName: self newClassName
		yourself
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> newClassName [

	^ newClassName
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> newClassName: anObject [

	newClassName := anObject
]

{ #category : 'execution' }
ReDuplicateClassDriver >> requestNewClass [

	^ requestDialog ifNil: [
		requestDialog := (StClassAndMethodsSelectionPresenter new
			label: 'Copy class ' , className 
			withInstanceMethods: rbClass methods
			withClassMethods: rbClass class methods) ]
]

{ #category : 'execution' }
ReDuplicateClassDriver >> runRefactoring [

	| dialog selectedMethods |
	self configureRefactoring.
	self requestNewClass openModal.
	requestDialog cancelled ifTrue: [ ^ nil ].
	self halt.
	selectedMethods := requestDialog selectedItems collect: [ :each | each name ].
	refactoring copyClass: className withName: dialog newClass.
	"self configureMessage."
	self applyChanges 
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	rbClass := model classFor: model environment classes anyOne.
	className := rbClass realClass name
]
