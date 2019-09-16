"
A basic miller presenter that wraps a non-miller presenter and does nothing on activation.
Useful to wrap elements that do not need to flow to the right.
"
Class {
	#name : #SpNullMillerPresenter,
	#superclass : #SpMillerPresenter,
	#instVars : [
		'wrappedPresenter'
	],
	#category : #'Spec2-Core-Miller'
}

{ #category : #specs }
SpNullMillerPresenter class >> defaultSpec [
	
	^ SpBoxLayout newHorizontal
		add: #wrappedPresenter;
		yourself
]

{ #category : #initialization }
SpNullMillerPresenter >> initializePresenters [
	
	"Do nothing"
]

{ #category : #initialization }
SpNullMillerPresenter >> setModelBeforeInitialization: aModel [

	wrappedPresenter := aModel.
	wrappedPresenter owner: self
]

{ #category : #initialization }
SpNullMillerPresenter >> whenActivatedDo: aBlock [

	"Do nothing"
]

{ #category : #initialization }
SpNullMillerPresenter >> wrappedPresenter [

	^ wrappedPresenter
]
