Class {
	#name : 'RBMakeClassAbstractDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBMakeClassAbstractDriver >> changes [
	"Remember should not call generateChanges"

	refactoring privateTransform.
	^ refactoring changes
]

{ #category : 'resources' }
RBMakeClassAbstractDriver >> configureRefactoring [
	"Configure the transformation"

	refactoring := RBMakeClassAbstractTransformation new
		model: model;
		classNamed: class name;
		yourself.
]

{ #category : 'execution' }
RBMakeClassAbstractDriver >> runRefactoring [
	"Run the transformation"

	| preconditions |
	
	self configureRefactoring.
	preconditions := refactoring failedApplicabilityPreconditions.
	preconditions
		ifNotEmpty: [ ^ self inform: preconditions first errorString ].
	self applyChanges.
]

{ #category : 'accessing' }
RBMakeClassAbstractDriver >> scopes: refactoringScopes class: aClass [ 

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	class := aClass
]
