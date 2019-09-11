"
I am an abstract base class for class refactorings.

All that I provide is the class name, my subclass refactorings are operating on, and a instance creation method
for setting the class name and an initial namespace model.

RBClassRefactoring model:className: 


"
Class {
	#name : #RBClassRefactoring,
	#superclass : #RBRefactoring,
	#instVars : [
		'className'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBClassRefactoring class >> className: aName [
	^self new className: aName
]

{ #category : #'instance creation' }
RBClassRefactoring class >> model: aRBSmalltalk className: aName [ 
	^(self new)
		model: aRBSmalltalk;
		className: aName;
		yourself
]

{ #category : #initialization }
RBClassRefactoring >> className: aName [
	className := aName
]
