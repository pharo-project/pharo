"
I am the component of DynamycSpecExample used for Integer.

self example 
"
Class {
	#name : #SpOpenOnIntExample,
	#superclass : #SpPresenter,
	#instVars : [
		'text',
		'plus',
		'minus'
	],
	#category : #'Spec2-Examples-Wrapper'
}

{ #category : #specs }
SpOpenOnIntExample class >> defaultSpec [
	<spec: #default>
	^ SpLayout composed
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
SpOpenOnIntExample class >> example [
	<sampleInstance>
	| example |
	example := (self new: 42).
	example extent: 100@100; openWithSpec.
	^example
]

{ #category : #instantiation }
SpOpenOnIntExample class >> new: anInteger [
	^ self new initialize: anInteger
]

{ #category : #initialization }
SpOpenOnIntExample >> initialize: anInteger [
	text label: anInteger asString
]

{ #category : #initialization }
SpOpenOnIntExample >> initializePresenter [
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
SpOpenOnIntExample >> initializeWidgets [
	text := self instantiate: SpLabelPresenter.
	plus := self instantiate: SpButtonPresenter.
	minus := self instantiate: SpButtonPresenter.
	plus
		label: '+'.
	minus
		label: '-'
]

{ #category : #accessing }
SpOpenOnIntExample >> minus [
	^ minus
]

{ #category : #accessing }
SpOpenOnIntExample >> plus [
	^ plus
]

{ #category : #accessing }
SpOpenOnIntExample >> text [
	^ text
]

{ #category : #accessing }
SpOpenOnIntExample >> title [
	^ 'OpenOnIntExample'
]
