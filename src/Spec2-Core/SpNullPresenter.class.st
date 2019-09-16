"
A null presenter that contains nothing.
Used by DynamicPresentersListBuilder to fill empty space when required for the layout.
"
Class {
	#name : #SpNullPresenter,
	#superclass : #SpPresenter,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpNullPresenter class >> defaultSpec [
	"Returns empty layout. That's the point of null presenter."

	^ SpBoxLayout newVertical
]

{ #category : #initialization }
SpNullPresenter >> initializePresenters [
	"Does nothing. That's the point of null presenter."
]
