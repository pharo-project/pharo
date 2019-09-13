"
A TestingComposablePresenter is a stupid composable model used to test SpecInterpreter.
"
Class {
	#name : #SpTestingPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'list'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #specs }
SpTestingPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #list;
		yourself
]

{ #category : #specs }
SpTestingPresenter class >> title [

	^ 'You should not see me !'
]

{ #category : #accessing }
SpTestingPresenter >> getText [
	^ Text new
]

{ #category : #initialization }
SpTestingPresenter >> initializePresenters [
	list := self newList
]

{ #category : #accessing }
SpTestingPresenter >> list [

	^ list
]
