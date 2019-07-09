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
		'#showOptions => SpObservableSlot',
		'#showOnlyDestination => SpObservableSlot',
		'#showOnlySource => SpObservableSlot',
		'#contextClass => SpObservableSlot',
		'#leftLabel => SpObservableSlot',
		'#leftText => SpObservableSlot',
		'#rightLabel => SpObservableSlot',
		'#rightText => SpObservableSlot'
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
	^ contextClass
]

{ #category : #accessing }
SpDiffPresenter >> contextClass: anObject [
	contextClass := anObject
]

{ #category : #initialization }
SpDiffPresenter >> initialize [
	super initialize.

	leftText := ''.
	rightText := ''.
	showOptions := true.
	showOnlyDestination := false.
	showOnlySource := false.

	self property: #leftText whenChangedDo: [ :newText | self changed: #leftText: with: {newText} ].
	self property: #rightText whenChangedDo: [ :newText | self changed: #rightText: with: {newText} ].
	self property: #contextClass whenChangedDo: [ :newClass | self changed: #contextClass: with: {newClass} ].
	self property: #showOptions whenChangedDo: [ :aBoolean | self changed: #showOptions: with: {aBoolean} ].
	self property: #showOnlyDestination whenChangedDo: [ :aBoolean | self changed: #showOnlyDestination: with: {aBoolean} ].
	self property: #showOnlySource whenChangedDo: [ :aBoolean | self changed: #showOnlySource: with: {aBoolean} ].
	self property: #leftLabel whenChangedDo: [ :newText | self changed: #leftLabel: with: {newText} ].
	self property: #rightLabel whenChangedDo: [ :newText | self changed: #rightLabel: with: {newText} ]
]

{ #category : #accessing }
SpDiffPresenter >> leftLabel [
	^ leftLabel
]

{ #category : #accessing }
SpDiffPresenter >> leftLabel: anObject [
	leftLabel := anObject
]

{ #category : #accessing }
SpDiffPresenter >> leftText [
	^ leftText
]

{ #category : #accessing }
SpDiffPresenter >> leftText: anObject [
	leftText := anObject
]

{ #category : #accessing }
SpDiffPresenter >> rightLabel [
	^ rightLabel
]

{ #category : #accessing }
SpDiffPresenter >> rightLabel: anObject [
	rightLabel := anObject
]

{ #category : #accessing }
SpDiffPresenter >> rightText [
	^ rightText
]

{ #category : #accessing }
SpDiffPresenter >> rightText: anObject [
	rightText := anObject
]

{ #category : #api }
SpDiffPresenter >> showBoth [
	self property: #showOnlySource rawValue: false.
	self property: #showOnlyDestination rawValue: false.
	self changed: #showBoth with: {}
]

{ #category : #api }
SpDiffPresenter >> showOnlyDestination [
	^ showOnlyDestination
]

{ #category : #api }
SpDiffPresenter >> showOnlyDestination: aBoolean [
	showOnlyDestination := aBoolean
]

{ #category : #api }
SpDiffPresenter >> showOnlySource [
	^ showOnlySource
]

{ #category : #api }
SpDiffPresenter >> showOnlySource: aBoolean [
	showOnlySource := aBoolean
]

{ #category : #api }
SpDiffPresenter >> showOptions [
	^ showOptions
]

{ #category : #api }
SpDiffPresenter >> showOptions: aBoolean [
	showOptions := aBoolean
]
