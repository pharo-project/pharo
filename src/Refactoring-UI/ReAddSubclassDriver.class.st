Class {
	#name : 'ReAddSubclassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'superclass',
		'subclass'
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

	refactoring := (RBInsertNewClassRefactoring className: self subclass)
		model: model;
		superclass: self superclass;
		subclasses: self superclass subclasses;
		packageName: self superclass packageName;
		tagName: self superclass tagName;
		yourself
]

{ #category : 'execution' }
ReAddSubclassDriver >> runRefactoring [

	self configureRefactoring.
	refactoring applicabilityPreconditions check
		ifFalse: [ ^ self inform: 'Preconditions not met' ].
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
