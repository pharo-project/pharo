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
	#superclass : 'ReClassInteractionDriver',
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

{ #category : 'introspection' }
ReDuplicateClassDriver >> className [
	"Answer a <String> representing the class to be copied"
	
	^ className
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> className: aClassName [

	className := aClassName.

]

{ #category : 'accessing' }
ReDuplicateClassDriver >> configureRefactoring [
	" This verification is necessary to make tests work "
	refactoring ifNil: [ refactoring := ReCopyClassRefactoring new ]
]

{ #category : 'private' }
ReDuplicateClassDriver >> fillFromDialog: aDialog [

	| presenter |
	presenter := aDialog presenter.

	self
		packageName: presenter packageName;
		tagName: presenter tagName;
		newClassName: presenter newClassName.

	refactoring
		packageName: presenter packageName;
		tagName: presenter tagName;
		className: presenter newClassName;
		sourceClass: className;
		instanceMethods: presenter selectedInstanceMethods;
		classMethods: presenter selectedClassMethods
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
		requestDialog := (StClassAndMethodsSelectionPresenter on: self)
			withInstanceMethods: rbClass methods
			withClassMethods: rbMetaclass allMethods;
			onCancel: [ : dialog | dialog close ];
			onAccept: [ :dialog |
				self fillFromDialog: dialog.

				dialog close ];
			selectAll;
			openModal;
			yourself ]
		
		ifNotNil: [ 
			self fillFromDialog: requestDialog ]
]

{ #category : 'execution' }
ReDuplicateClassDriver >> runRefactoring [

	self configureRefactoring.
	self requestNewClass.
	requestDialog window isCancelled
		ifTrue: [ ^ nil ].
	self applyChanges 
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	
	rbClass := model classNamed: self className.
	rbMetaclass := model classNamed: self className, ' class'.
	packageName := rbClass packageName
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> tagName [

	^ tagName
]
