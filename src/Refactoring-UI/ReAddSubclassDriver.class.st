Class {
	#name : 'ReAddSubclassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'superclass',
		'subclass',
		'comment',
		'tagName',
		'packageName',
		'packageNames'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'instance creation' }
ReAddSubclassDriver class >> superclass: aClass packageNames: packages [

	^ self basicNew
		superclass: aClass;
		packageNames: packages;
		initialize;
		yourself
]

{ #category : 'execution' }
ReAddSubclassDriver >> changes [
	"Remember should not call generateChanges"

	refactoring privateTransform.
	^ refactoring changes
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

	refactoring := (RBAddNewClassRefactoring className: self subclass)
		model: model;
		superclass: self superclass;
		packageName: self packageName;
		tagName: self tagName;
		comment: self comment;
		yourself
]

{ #category : 'initialization' }
ReAddSubclassDriver >> initialize [

	super initialize.
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

{ #category : 'accessing' }
ReAddSubclassDriver >> packageName [
	"Answer a <Symbol> representing the receiver's package name where the new class will be add"

	^ packageName
		ifNil: [ self superclass packageName ] 

]

{ #category : 'accessing' }
ReAddSubclassDriver >> packageName: anObject [

	packageName := anObject
]

{ #category : 'accessing' }
ReAddSubclassDriver >> packageNames [

	^ packageNames
]

{ #category : 'accessing' }
ReAddSubclassDriver >> packageNames: anObject [

	packageNames := anObject
]

{ #category : 'execution' }
ReAddSubclassDriver >> runRefactoring [

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ : conditions | ^ self informConditions: conditions ].
	self applyChanges.
]

{ #category : 'accessing' }
ReAddSubclassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	superclass := model classFor: model environment classes anyOne.
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

{ #category : 'accessing' }
ReAddSubclassDriver >> tagName: anObject [

	tagName := anObject
]
