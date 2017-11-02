"
A TestingComposablePresenter is a stupid composable model used to test SpecInterpreter.
"
Class {
	#name : #TestingComposablePresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'list'
	],
	#category : 'Spec-Tests-Utils'
}

{ #category : #specs }
TestingComposablePresenter class >> defaultSpec [
	<spec>

	^ SpecLayout composed
		add: #list;
		yourself
]

{ #category : #specs }
TestingComposablePresenter class >> testingSpec [
	^ {#RubPluggableTextMorph. #model:. #model. }
]

{ #category : #specs }
TestingComposablePresenter class >> title [

	^ 'You should not see me !'
]

{ #category : #accessing }
TestingComposablePresenter >> getText [
	^ Text new
]

{ #category : #initialization }
TestingComposablePresenter >> initializeWidgets [
	list := self newList
]

{ #category : #accessing }
TestingComposablePresenter >> list [

	^ list
]
