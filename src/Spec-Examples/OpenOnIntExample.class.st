"
I am the component of DynamycSpecExample used for Integer.

self example 
"
Class {
	#name : #OpenOnIntExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'text',
		'plus',
		'minus'
	],
	#category : #Spec-Examples-Wrapper
}

{ #category : #specs }
OpenOnIntExample class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column 
				add: #text height: 25;
				newRow: [ :row | 
					row
						add: #minus;
						addSplitter;
						add: #plus ]
					height: 25 ];
		yourself
]

{ #category : #examples }
OpenOnIntExample class >> example [
	| example |
	example := (self new: 42).
	example extent: 100@100; openWithSpec.
	^example
]

{ #category : #instantiation }
OpenOnIntExample class >> new: anInteger [
	^ self new initialize: anInteger
]

{ #category : #initialization }
OpenOnIntExample >> initialize: anInteger [
	text label: anInteger asString
]

{ #category : #initialization }
OpenOnIntExample >> initializePresenter [
	minus
		action: [ | currentValue |
			currentValue := text label asInteger.
			text label: (currentValue - 1) asString ].
	plus
		action: [ | currentValue |
			currentValue := text label asInteger.
			text label: (currentValue + 1) asString ]
]

{ #category : #initialization }
OpenOnIntExample >> initializeWidgets [
	text := self instantiate: LabelPresenter.
	plus := self instantiate: ButtonPresenter.
	minus := self instantiate: ButtonPresenter.
	plus
		label: '+'.
	minus
		label: '-'
]

{ #category : #accessing }
OpenOnIntExample >> minus [
	^ minus
]

{ #category : #accessing }
OpenOnIntExample >> plus [
	^ plus
]

{ #category : #accessing }
OpenOnIntExample >> text [
	^ text
]

{ #category : #accessing }
OpenOnIntExample >> title [
	^ 'OpenOnIntExample'
]
