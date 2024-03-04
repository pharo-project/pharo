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

{ #category : 'accessing' }
RBMakeClassAbstractDriver >> class: anObject [

	class := anObject
]

{ #category : 'resources' }
RBMakeClassAbstractDriver >> configureRefactoring [
	"Configure the transformation"

	refactoring := RBMakeClassAbstractTransformation new
					class: class;
					yourself.
]

{ #category : 'execution' }
RBMakeClassAbstractDriver >> runRefactoring [
	"Run the transformation"

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions
		ifNotEmpty: [ ^ self inform: 'Preconditions not met' ].
	self applyChanges.
]
