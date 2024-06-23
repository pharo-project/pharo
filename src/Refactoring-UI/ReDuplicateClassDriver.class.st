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
		'rbClass',
		'rbMetaclass'
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
		requestDialog := (StClassAndMethodsSelectionPresenter
			withInstanceMethods: rbClass methods
			withClassMethods: rbMetaclass allMethods)
				label: 'Copy class ' , className;
				selectAll ]
]

{ #category : 'execution' }
ReDuplicateClassDriver >> runRefactoring [

	self configureRefactoring.
	self requestNewClass openModal.
	requestDialog cancelled ifTrue: [ ^ nil ].

	refactoring 
		className: requestDialog newClassName;
		sourceClass: className;
		instanceMethods: requestDialog selectedInstanceMethods;
		classMethods: requestDialog selectedClassMethods;
		copySelectedMethods.

	"self configureMessage."
	self applyChanges 
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> scopes: refactoringScopes [

	| selectedClass |
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	selectedClass := model environment classes anyOne.	

	rbClass := model classFor: selectedClass.
	rbMetaclass := model classFor: selectedClass class.
	className := rbClass realClass name
]
