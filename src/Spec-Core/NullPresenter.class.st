"
A null presenter that contains nothing.
Used by DynamicPresentersListBuilder to fill empty space when required for the layout.
"
Class {
	#name : #NullPresenter,
	#superclass : #ComposablePresenter,
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
NullPresenter class >> defaultSpec [
	"Returns empty layout. That's the point of null presenter."
	^ SpecLayout composed
]

{ #category : #initialization }
NullPresenter >> initializeWidgets [
	"Does nothing. That's the point of null presenter."
]
