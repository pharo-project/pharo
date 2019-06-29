"
I am a Spec widget useful for visualising differences between two strings.

When a classContext: is setted, the strings are highlighted using such contextual information.

Examples:

self exampleWithoutOptions.
self exampleWithOptions.
"
Class {
	#name : #SpDiffPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'leftTextHolder',
		'rightTextHolder',
		'showOptions',
		'contextClassHolder',
		'showOnlyDestination',
		'showOnlySource',
		'leftLabelHolder',
		'rightLabelHolder'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpDiffPresenter class >> adapterName [

	^ #DiffAdapter
]

{ #category : #'undo-redo' }
SpDiffPresenter >> clearUndoManager [
	"The diff presenter does not allow edition, it does not have undo history. But this message is added to have polimorphism in the Message Browser"
]

{ #category : #accessing }
SpDiffPresenter >> contextClass [

	^ contextClassHolder value
]

{ #category : #accessing }
SpDiffPresenter >> contextClass: anObject [

	contextClassHolder value: anObject 
]

{ #category : #initialization }
SpDiffPresenter >> initialize [

	super initialize.
	
	leftTextHolder := '' asValueHolder.
	rightTextHolder := '' asValueHolder.
	contextClassHolder := nil asValueHolder.
	showOptions := true asValueHolder.
	showOnlyDestination := false asValueHolder.
	showOnlySource := false asValueHolder.	
	leftLabelHolder := nil asValueHolder.
	rightLabelHolder := nil asValueHolder.
		
	leftTextHolder whenChangedDo: [ :newText | self changed: #leftText: with: { newText } ].
	rightTextHolder whenChangedDo: [ :newText | self changed: #rightText: with: { newText } ].
	contextClassHolder whenChangedDo: [ :newClass | self changed: #contextClass: with: { newClass } ].	
	showOptions whenChangedDo: [ :aBoolean | self changed: #showOptions: with: { aBoolean }].	
	showOnlyDestination whenChangedDo: [:aBoolean | self changed: #showOnlyDestination: with: { aBoolean } ].
	showOnlySource whenChangedDo: [:aBoolean | self changed: #showOnlySource: with: { aBoolean } ].
	
	leftLabelHolder whenChangedDo: [:newText | self changed: #leftLabel: with: { newText } ].
	rightLabelHolder whenChangedDo: [:newText | self changed: #rightLabel: with: { newText } ].

]

{ #category : #accessing }
SpDiffPresenter >> leftLabel [

	^ leftLabelHolder value
]

{ #category : #accessing }
SpDiffPresenter >> leftLabel: anObject [

	leftLabelHolder value: anObject 
]

{ #category : #accessing }
SpDiffPresenter >> leftText [

	^ leftTextHolder value
]

{ #category : #accessing }
SpDiffPresenter >> leftText: anObject [

	leftTextHolder value: anObject 
]

{ #category : #accessing }
SpDiffPresenter >> rightLabel [

	^ rightLabelHolder value
]

{ #category : #accessing }
SpDiffPresenter >> rightLabel: anObject [

	rightLabelHolder value: anObject 
]

{ #category : #accessing }
SpDiffPresenter >> rightText [

	^ rightTextHolder value
]

{ #category : #accessing }
SpDiffPresenter >> rightText: anObject [

	rightTextHolder value: anObject 
]

{ #category : #api }
SpDiffPresenter >> showBoth [

	showOnlySource rawValue: false.
	showOnlyDestination rawValue: false.
	self changed: #showBoth with: { }
]

{ #category : #api }
SpDiffPresenter >> showOnlyDestination [

	^ showOnlyDestination value
]

{ #category : #api }
SpDiffPresenter >> showOnlyDestination: aBoolean [

	showOnlyDestination rawValue: false.
	showOnlyDestination value: aBoolean.
]

{ #category : #api }
SpDiffPresenter >> showOnlySource [

	^ showOnlySource value
]

{ #category : #api }
SpDiffPresenter >> showOnlySource: aBoolean [

	showOnlyDestination rawValue: false.
	showOnlySource value: aBoolean
]

{ #category : #api }
SpDiffPresenter >> showOptions [

	^ showOptions value
]

{ #category : #api }
SpDiffPresenter >> showOptions: aBoolean [

	showOptions value: aBoolean
]
