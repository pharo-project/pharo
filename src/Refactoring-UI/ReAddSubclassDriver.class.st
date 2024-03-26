Class {
	#name : 'ReAddSubclassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'superclass',
		'subclass',
		'package',
		'tag'
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
ReAddSubclassDriver >> configureRefactoring [

	refactoring := (RBAddNewClassRefactoring className: self subclass)
		model: model;
		superclass: self superclass;
		subclasses: self superclass subclasses;
		packageName: self packageName;
		tagName: self tag name;
		yourself
]

{ #category : 'accessing' }
ReAddSubclassDriver >> package [

	^ package
]

{ #category : 'accessing' }
ReAddSubclassDriver >> package: anObject [

	package := anObject
]

{ #category : 'accessing' }
ReAddSubclassDriver >> packageName [
	"Answer a <Symbol> representing the receiver's package name where the new class will be add"

	^ self package 
		ifNil: [ self superclass packageName ] 
		ifNotNil: [ self package name ]
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
	model := self refactoringScopeOn: scopes first.
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
ReAddSubclassDriver >> tag [

	^ tag
]

{ #category : 'accessing' }
ReAddSubclassDriver >> tag: aPackageTag [ 

	tag := aPackageTag
]
