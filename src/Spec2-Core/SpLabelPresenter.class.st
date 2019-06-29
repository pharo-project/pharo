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
		'labelHolder',
		'emphasis'
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

{ #category : #initialization }
SpLabelPresenter >> defaultColor [
	^ self theme textColor
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

	labelHolder := '' asValueHolder.
	emphasis := #().
	labelHolder whenChangedDo: [ self changed: #getText ]
]

{ #category : #api }
SpLabelPresenter >> label [
	"<api: #inspect>"

	^ labelHolder value
]

{ #category : #api }
SpLabelPresenter >> label: aText [
	"<api: #string getter: #label>"
	"Set the text of the label"
	
	labelHolder value: aText
]

{ #category : #'api-valueHolder' }
SpLabelPresenter >> labelHolder [
	"To register actions to be done when the label changes."
	^ labelHolder
]

{ #category : #'api-events' }
SpLabelPresenter >> whenLabelChangedDo: aValuable [

	labelHolder whenChangedDo: aValuable
]
