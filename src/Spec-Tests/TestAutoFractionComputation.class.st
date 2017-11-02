"
I am a widget used to verify that the mechanism used to compute rows and columns are still working.
"
Class {
	#name : #TestAutoFractionComputation,
	#superclass : #ComposablePresenter,
	#instVars : [
		'button',
		'list',
		'text'
	],
	#category : 'Spec-Tests-Utils'
}

{ #category : #spec }
TestAutoFractionComputation class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		newRow: [ :r |
			r 
				add: #list left: 0.8;
				newColumn: [ :c |
					c 
						add: #button bottom: 0.7;
						add: #text top: 0.3 ] right: 0.2 ];
		yourself
]

{ #category : #accessing }
TestAutoFractionComputation >> button [
	^ button
]

{ #category : #initialization }
TestAutoFractionComputation >> initializeWidgets [
	button := self newButton.
	list := self newList.
	text := self newText
]

{ #category : #accessing }
TestAutoFractionComputation >> list [
	^ list
]

{ #category : #accessing }
TestAutoFractionComputation >> text [
	^ text
]
