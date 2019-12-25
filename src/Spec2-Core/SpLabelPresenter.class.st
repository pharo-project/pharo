"
A LabelPresenter is a spec presenter for Labels.
See AbstractWidgetPresenter

self example

I provide the following variables and their accessors
- label is the text displayed that I display.

I am specializing defaultColor to change the color of my text.

todo
- emphasis
"
Class {
	#name : #SpLabelPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#emphasis',
		'#label => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpLabelPresenter class >> adapterName [

	^ #LabelAdapter
]

{ #category : #specs }
SpLabelPresenter class >> title [

	^ 'Label presenter'
]

{ #category : #testing }
SpLabelPresenter >> canTakeKeyboardFocus [

	^ false
]

{ #category : #initialization }
SpLabelPresenter >> defaultColor [
	^ self theme textColor
]

{ #category : #initialization }
SpLabelPresenter >> defineInputPorts [

	^ { SpLabelPort new }
]

{ #category : #api }
SpLabelPresenter >> emphasis [
	^ emphasis
]

{ #category : #api }
SpLabelPresenter >> emphasis: anEmphasis [
	"accepts a symbol or an array of symbols: #bold, #italic, #underline, #narrow, #struck"

	emphasis := anEmphasis isSymbol
		ifTrue: [ {anEmphasis} ]
		ifFalse: [ anEmphasis ].
	self changed: #emphasis: with: {emphasis}
]

{ #category : #initialization }
SpLabelPresenter >> initialize [
	super initialize.

	label := ''.
	emphasis := #().
	self whenLabelChangedDo: [ self changed: #getText ]
]

{ #category : #api }
SpLabelPresenter >> label [
	^ label
]

{ #category : #api }
SpLabelPresenter >> label: aText [
	"Set the text of the label"

	label := aText
]

{ #category : #'api-events' }
SpLabelPresenter >> whenLabelChangedDo: aValuable [
	self property: #label whenChangedDo: aValuable
]
