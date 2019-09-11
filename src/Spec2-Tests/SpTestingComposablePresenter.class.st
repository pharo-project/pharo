"
A TestingComposablePresenter is a stupid composable model used to test SpecInterpreter.
"
Class {
	#name : #SpTestingComposablePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'list'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #specs }
SpTestingComposablePresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #list;
		yourself
]

{ #category : #specs }
SpTestingComposablePresenter class >> title [

	^ 'You should not see me !'
]

{ #category : #accessing }
SpTestingComposablePresenter >> getText [
	^ Text new
]

{ #category : #initialization }
SpTestingComposablePresenter >> initializeWidgets [
	list := self newList
]

{ #category : #accessing }
SpTestingComposablePresenter >> list [

	^ list
]
