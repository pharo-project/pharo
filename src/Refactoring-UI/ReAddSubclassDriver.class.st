"
Interaction object responsible for driving the analysis and interactive adding of classes.

The driver could be instantiated by providing a superclass, and additionally, a `Collection` of package names.

"
Class {
	#name : 'ReAddSubclassDriver',
	#superclass : 'ReClassInteractionDriver',
	#instVars : [
		'superclass',
		'subclass',
		'comment'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing - class hierarchy' }
ReAddSubclassDriver class >> superclass: aClass [
	"superclass is the class that acts as a reference. A new subclass will be inserted below"

	^ self basicNew
		superclass: aClass;
		initialize;
		yourself
]

{ #category : 'instance creation' }
ReAddSubclassDriver class >> superclass: aClass packageNames: packages [
	"Answer a new instance using aClass as the superclass of the receiver's subclass, and packages as a <Collection> of <String> specifying the package names where the new subclass will be created"

	^ self basicNew
		superclass: aClass;
		packageNames: packages;
		initialize;
		yourself
]

{ #category : 'accessing' }
ReAddSubclassDriver >> comment [

	^ comment
]

{ #category : 'accessing' }
ReAddSubclassDriver >> comment: aString [ 

	comment := aString
]

{ #category : 'accessing' }
ReAddSubclassDriver >> configureRefactoring [


	refactoring := (RBInsertNewClassRefactoring className: self subclass)
		model: model;
		superclass: self superclass;
		packageName: self packageName;
		tagName: self tagName;
		comment: self comment;
		yourself
]

{ #category : 'accessing' }
ReAddSubclassDriver >> dialogTitleForClassSelection [

	^ 'Add new subclass of ' , superclass name
]

{ #category : 'accessing' }
ReAddSubclassDriver >> packageName [
	"Answer a <Symbol> representing the receiver's package name where the new class will be add"

	^ packageName
		ifNil: [ self superclass packageName ] 

]

{ #category : 'initialization' }
ReAddSubclassDriver >> requestSingleSubclass [

	(StRefactoringAddClassPresenter on: self)
		onCancel: [ : dialog | dialog close ];
		onAccept: [ :dialog |
			self
				packageName: dialog presenter packageName;
				tagName: dialog presenter tagName;
				comment: dialog presenter commentPresenterText.
			dialog close ];
		openModal
]

{ #category : 'execution' }
ReAddSubclassDriver >> runRefactoring [
	
	subclass ifNil: [ 
		self requestSingleSubclass ].
	"the refactoring was cancelled."
	subclass ifNil: [ ^ false ].
	
	self configureRefactoring.
	
	refactoring failedApplicabilityPreconditions
		ifNotEmpty: [ : conditions | self informConditions: conditions. ^ false ].
	self applyChanges.
	^ true
]

{ #category : 'accessing' }
ReAddSubclassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	superclass := model classObjectFor: superclass

]

{ #category : 'accessing' }
ReAddSubclassDriver >> subclass [

	^ subclass
]

{ #category : 'accessing' }
ReAddSubclassDriver >> subclass: aClass [

	subclass := aClass
]

{ #category : 'accessing' }
ReAddSubclassDriver >> superclass [

	^ superclass
]

{ #category : 'accessing' }
ReAddSubclassDriver >> superclass: anObject [

	superclass := anObject
]

{ #category : 'accessing' }
ReAddSubclassDriver >> tagName [
	"Answer a <Symbol> representing the receiver's tag name where the new class will be add"

	^ tagName
		ifNil: [ self superclass tagName ]

]
