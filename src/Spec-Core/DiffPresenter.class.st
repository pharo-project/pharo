"
I am a Spec widget useful for visualising differences between two strings.

When a classContext: is setted, the strings are highlighted using such contextual information.

Examples:

self exampleWithoutOptions.
self exampleWithOptions.
"
Class {
	#name : #DiffPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'leftTextHolder',
		'rightTextHolder',
		'showOptions',
		'contextClassHolder',
		'showOnlyDestination',
		'showOnlySource'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
DiffPresenter class >> adapterName [

	^ #DiffAdapter
]

{ #category : #specs }
DiffPresenter class >> defaultSpec [
	<spec>
	
	^ #(MorphicDiffAdapter
		adapt: #(model))
]

{ #category : #example }
DiffPresenter class >> exampleWithOptions [
	"
	self exampleWithOptions
	"

	^ self new
		showOptions: true;
		leftText: (True >> #and:) sourceCode;
		rightText: (True >> #or:) sourceCode;
		contextClass: True;
		openWithSpec
]

{ #category : #example }
DiffPresenter class >> exampleWithoutOptions [
	"
	self exampleWithoutOptions
	"

	^ self new
		showOptions: false;
		leftText: 'Old text';
		rightText: 'New text';
		openWithSpec

]

{ #category : #'undo-redo' }
DiffPresenter >> clearUndoManager [
	"The diff presenter does not allow edition, it does not have undo history. But this message is added to have polimorphism in the Message Browser"
]

{ #category : #accessing }
DiffPresenter >> contextClass [

	^ contextClassHolder value
]

{ #category : #accessing }
DiffPresenter >> contextClass: anObject [

	contextClassHolder value: anObject 
]

{ #category : #initialization }
DiffPresenter >> initialize [

	super initialize.
	
	leftTextHolder := '' asValueHolder.
	rightTextHolder := '' asValueHolder.
	contextClassHolder := nil asValueHolder.
	showOptions := true asValueHolder.
	showOnlyDestination := false asValueHolder.
	showOnlySource := false asValueHolder.	
		
	leftTextHolder whenChangedDo: [ :newText | self changed: #leftText: with: { newText } ].
	rightTextHolder whenChangedDo: [ :newText | self changed: #rightText: with: { newText } ].
	contextClassHolder whenChangedDo: [ :newClass | self changed: #contextClass: with: { newClass } ].	
	showOptions whenChangedDo: [ :aBoolean | self changed: #showOptions: with: { aBoolean }].	
	showOnlyDestination whenChangedDo: [:aBoolean | self changed: #showOnlyDestination: with: { aBoolean } ].
	showOnlySource whenChangedDo: [:aBoolean | self changed: #showOnlySource: with: { aBoolean } ]
]

{ #category : #accessing }
DiffPresenter >> leftText [

	^ leftTextHolder value
]

{ #category : #accessing }
DiffPresenter >> leftText: anObject [

	leftTextHolder value: anObject 
]

{ #category : #accessing }
DiffPresenter >> rightText [

	^ rightTextHolder value
]

{ #category : #accessing }
DiffPresenter >> rightText: anObject [

	rightTextHolder value: anObject 
]

{ #category : #api }
DiffPresenter >> showBoth [

	showOnlySource rawValue: false.
	showOnlyDestination rawValue: false.
	self changed: #showBoth with: { }
]

{ #category : #api }
DiffPresenter >> showOnlyDestination [

	^ showOnlyDestination value
]

{ #category : #api }
DiffPresenter >> showOnlyDestination: aBoolean [

	showOnlyDestination rawValue: false.
	showOnlyDestination value: aBoolean.
]

{ #category : #api }
DiffPresenter >> showOnlySource [

	^ showOnlySource value
]

{ #category : #api }
DiffPresenter >> showOnlySource: aBoolean [

	showOnlyDestination rawValue: false.
	showOnlySource value: aBoolean
]

{ #category : #api }
DiffPresenter >> showOptions [

	^ showOptions value
]

{ #category : #api }
DiffPresenter >> showOptions: aBoolean [

	showOptions value: aBoolean
]
