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
		'rbMetaclass',
		'superclassName'
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
ReDuplicateClassDriver class >> driverName [

	^ 'Duplicate class'
]

{ #category : 'accessing' }
ReDuplicateClassDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'private' }
ReDuplicateClassDriver >> acceptClassRequest: dialog [

	self
		packageName: dialog presenter packageName;
		tagName: dialog presenter tagName;
		newClassName: dialog presenter newClassName;
		superclassName: dialog presenter selectedSuperclass.
	
	refactoring
		packageName: dialog presenter packageName;
		className: dialog presenter newClassName;
		superclassName: dialog presenter selectedSuperclassName;					
		sourceClass: className;
		instanceMethods: dialog presenter selectedInstanceMethods;
		classMethods: dialog presenter selectedClassMethods;
		packageTagName: dialog presenter tagName.
]

{ #category : 'introspection' }
ReDuplicateClassDriver >> className [
	"Answer a <String> representing the class to be copied"
	
	^ className
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> className: aClassName [

	className := aClassName

]

{ #category : 'accessing' }
ReDuplicateClassDriver >> dialogTitleForClassSelection [

	^ 'Duplicate class ' , className
]

{ #category : 'execution' }
ReDuplicateClassDriver >> gatherUserInput [

	self requestNewClass.
	requestDialog window isCancelled ifTrue: [ ^ false ].
	refactoring
		superclassName: requestDialog selectedSuperclass name;
		className: requestDialog newClassName;
		packageName: requestDialog packageName;
		packageTagName: requestDialog tagName.

	^ true
]

{ #category : 'configuration' }
ReDuplicateClassDriver >> instantiateRefactoring [

	^ ReDuplicateClassRefactoring copyClass: rbClass
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> newClass [
	"Answer the new created <Class>"

	^ (self model classNamed: self newClassName) binding value
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> newClassName [
	"Answer a <Symbol> representing the new duplicated class"

	^ newClassName
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> newClassName: anObject [

	newClassName := anObject
]

{ #category : 'private' }
ReDuplicateClassDriver >> newRequestClassDialog [

	^ (StClassAndMethodsSelectionPresenter newApplication: self application model: self)
		withInstanceMethods: rbClass localMethods
		withClassMethods: rbMetaclass localMethods;
		baseClassName: self className;
		basePackageName: (self model environment at: self className) packageName;
		basePackageTagName: (self model environment at: self className) packageTag name;
		onCancel: [ : dialog | dialog close ];
		onAccept: [ :dialog |
			self acceptClassRequest: dialog.
			dialog close ];
		yourself
]

{ #category : 'private' }
ReDuplicateClassDriver >> requestNewClass [

	requestDialog ifNil: [ requestDialog := self newRequestClassDialog ].
	requestDialog selectAll.
	^ requestDialog
		openModal;
		yourself
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	rbClass := model classNamed: className.
	rbMetaclass := model metaclassNamed: className.
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> superclassName [

	^ superclassName
]

{ #category : 'accessing' }
ReDuplicateClassDriver >> superclassName: anObject [

	superclassName := anObject
]
