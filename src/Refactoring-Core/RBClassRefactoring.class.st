"
I am an abstract base class for class refactorings.

All that I provide is the class name, my subclass refactorings are operating on, and a instance creation method
for setting the class name and an initial namespace model.

Check method `RBClassRefactoring class>>#model:className:` 


"
Class {
	#name : 'RBClassRefactoring',
	#superclass : 'ReRefactoring',
	#instVars : [
		'className'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBClassRefactoring class >> className: aName [
	^ self new className: aName
]

{ #category : 'testing' }
RBClassRefactoring class >> isAbstract [

	^ self == RBClassRefactoring
]

{ #category : 'instance creation' }
RBClassRefactoring class >> model: aRBModel className: aName [
	^ self new
		model: aRBModel;
		className: aName;
		yourself
]

{ #category : 'initialization' }
RBClassRefactoring >> className: aName [
	className := aName
]
