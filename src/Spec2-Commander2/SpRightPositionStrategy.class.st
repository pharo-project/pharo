"
I set the button generated from a command on the left side a presenter.
"
Class {
	#name : #SpRightPositionStrategy,
	#superclass : #SpPositionStrategy,
	#category : #'Spec2-Commander2-Strategies'
}

{ #category : #adding }
SpRightPositionStrategy >> addButton: aButtonPresenter toActionBar: anActionBarPresenter [
	anActionBarPresenter addLast: aButtonPresenter
]

{ #category : #adding }
SpRightPositionStrategy >> addButton: aButtonPresenter toToolbar: aToolbarPresenter [
	aToolbarPresenter addItemRight: aButtonPresenter
]
