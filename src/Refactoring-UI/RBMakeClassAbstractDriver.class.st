<<<<<<< HEAD
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
		className: class name;
		class: (model classNamed: class name) value;
		yourself.
]

{ #category : 'execution' }
RBMakeClassAbstractDriver >> runRefactoring [
	"Run the transformation. Note that RB classes are abstract by default (see implementor of RBClass>>#isAbstract) but in the meta-object protocol model a class is NOT abstract unless implements the #isAbstract method returning <true>. This should be fixed at some point, but as this refactoring seems not to be widely used, let's prioritize other refactorings"

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions
		ifNotEmpty: [ ^ self inform: 'Preconditions not met' ].
	self applyChanges.
]

{ #category : 'accessing' }
RBMakeClassAbstractDriver >> scopes: refactoringScopes class: aClass [ 

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	class := aClass
]
||||||| ec979f6c13
=======
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

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions
		ifNotEmpty: [ ^ self inform: 'Preconditions not met' ].
	self applyChanges.
]

{ #category : 'accessing' }
RBMakeClassAbstractDriver >> scopes: refactoringScopes class: aClass [ 

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	class := aClass
]
>>>>>>> add_subclass_refactoring_driver
