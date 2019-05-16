"
A basic miller presenter that wraps a non-miller presenter and does nothing on activation.
Useful to wrap elements that do not need to flow to the right.
"
Class {
	#name : #SpecNullMillerPresenter,
	#superclass : #SpecMillerPresenter,
	#instVars : [
		'wrappedPresenter'
	],
	#category : #'Spec-Core-Miller'
}

{ #category : #specs }
SpecNullMillerPresenter class >> defaultSpec [
	
	^ SpecBoxLayout newHorizontal
		add: #wrappedPresenter;
		yourself
]

{ #category : #initialization }
SpecNullMillerPresenter >> initializeWidgets [
	
	"Do nothing"
]

{ #category : #initialization }
SpecNullMillerPresenter >> setModelBeforeInitialization: aModel [

	wrappedPresenter := aModel.
	wrappedPresenter owner: self
]

{ #category : #initialization }
SpecNullMillerPresenter >> whenActivatedDo: aBlock [

	"Do nothing"
]

{ #category : #initialization }
SpecNullMillerPresenter >> wrappedPresenter [

	^ wrappedPresenter
]
