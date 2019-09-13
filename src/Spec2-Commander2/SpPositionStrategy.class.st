"
I model a strategy to add a command in a presenter for which it can be either left or right.
"
Class {
	#name : #SpPositionStrategy,
	#superclass : #Object,
	#category : #'Spec2-Commander2-Strategies'
}

{ #category : #adding }
SpPositionStrategy >> addButton: aButtonPresenter toActionBar: anActionBarPresenter [
	self subclassResponsibility
]

{ #category : #adding }
SpPositionStrategy >> addButton: aButtonPresenter toToolbar: aToolbarPresenter [
	self subclassResponsibility
]
