"
I am the component of DynamycSpecExample used for String.

self example 

I show also how to dynamically redrawn a widget.
"
Class {
	#name : #SpOpenOnStringExample,
	#superclass : #SpPresenter,
	#instVars : [
		'label',
		'check',
		'input',
		'button',
		'textToReset'
	],
	#category : #'Spec2-Examples-Wrapper'
}

{ #category : #specs }
SpOpenOnStringExample class >> bottomLayout [
	<spec>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				newRow: [ :row | 
					row
						add: #input;
						add: #button ]
					height: 25;
				newRow: [ :row | 
					row
						add: #label;
						add: #check width: 100 ]
					height: 25 ];
		yourself
]

{ #category : #specs }
SpOpenOnStringExample class >> defaultSpec [
	<spec: #default>
	^ SpLayout composed
		newColumn: [ :column | 
			column
				newRow: [ :row | 
					row
						add: #label;
						add: #check width: 100 ]
					height: 25;
				newRow: [ :row | 
					row
						add: #input;
						add: #button ]
					height: 25 ];
		yourself
]

{ #category : #examples }
SpOpenOnStringExample class >> example [
	<sampleInstance>
	| example |
	example := self new: 'Hello world'.
	example
		extent: 300 @ 200;
		openWithSpec.
	^ example
]

{ #category : #instantiation }
SpOpenOnStringExample class >> new: aString [
	^ self new initialize: aString
]

{ #category : #accessing }
SpOpenOnStringExample >> button [
	^ button
]

{ #category : #accessing }
SpOpenOnStringExample >> check [
	^ check
]

{ #category : #initialization }
SpOpenOnStringExample >> initialize: aString [
	label label: aString.
	input text: aString.
	textToReset := aString
]

{ #category : #initialization }
SpOpenOnStringExample >> initializePresenter [
	button action: [ input text: textToReset ].
	input whenTextChangedDo: [ :text | label label: text ].
	check
		whenActivatedDo: [ self needRebuild: false.
			self buildWithSpecLayout: self class defaultSpec ];
		whenDeactivatedDo: [ self needRebuild: false.
			self buildWithSpecLayout: self class bottomLayout ]
]

{ #category : #initialization }
SpOpenOnStringExample >> initializeWidgets [
	label := self instantiate: SpLabelPresenter.
	check := self instantiate: SpCheckBoxPresenter.
	input := self instantiate: SpTextInputFieldPresenter.
	button := self instantiate: SpButtonPresenter.
	button
		label: 'reset';
		action: [ input text: textToReset ].
	check
		label: 'Label on top';
		state: true
]

{ #category : #accessing }
SpOpenOnStringExample >> input [
	^ input
]

{ #category : #accessing }
SpOpenOnStringExample >> label [
	^ label
]

{ #category : #'as yet unclassified' }
SpOpenOnStringExample >> openOnString [
	| ui temp object |
	object whenChangedDo: [ :o | ui label label: o asString ].
	temp := object value.

]

{ #category : #accessing }
SpOpenOnStringExample >> textToReset [
	^ textToReset
]

{ #category : #accessing }
SpOpenOnStringExample >> title [
	^ 'OpenOnStringExample'
]
