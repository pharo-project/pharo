Class {
	#name : 'ReAddSubclassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'superclass',
		'subclass',
		'comment',
		'tagName',
		'packageName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

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
		subclasses: self superclass subclasses;
		packageName: self packageName;
		tagName: self tagName;
		comment: self comment;
		yourself
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

{ #category : 'execution' }
ReAddSubclassDriver >> runRefactoring [

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ : conditions | ^ self informConditions: conditions ].
	self applyChanges.
]

{ #category : 'accessing' }
ReAddSubclassDriver >> scopes: refactoringScopes subclass: newSubclass [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	superclass := model classFor: model environment classes anyOne.
	subclass := newSubclass.
]

{ #category : 'accessing' }
ReAddSubclassDriver >> subclass [

	^ subclass
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
