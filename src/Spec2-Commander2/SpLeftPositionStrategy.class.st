"
I set the button generated from a command on the left side a presenter.
"
Class {
	#name : #SpLeftPositionStrategy,
	#superclass : #SpPositionStrategy,
	#category : #'Spec2-Commander2-Strategies'
}

{ #category : #adding }
SpLeftPositionStrategy >> addButton: aButtonPresenter toActionBar: anActionBarPresenter [
	anActionBarPresenter add: aButtonPresenter
]

{ #category : #adding }
SpLeftPositionStrategy >> addButton: aButtonPresenter toToolbar: aToolbarPresenter [
	aToolbarPresenter addItemLeft: aButtonPresenter
]
