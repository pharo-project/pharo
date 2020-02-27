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
	#name : #LabelPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'labelHolder',
		'emphasis'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
LabelPresenter class >> adapterName [

	^ #LabelAdapter
]

{ #category : #specs }
LabelPresenter class >> defaultSpec [
	<spec>
	
	^ #(LabelAdapter
		adapt: #(model))
]

{ #category : #specs }
LabelPresenter class >> title [

	^ 'Label Morph'
]

{ #category : #initialization }
LabelPresenter >> defaultColor [
	^ self theme textColor
]

{ #category : #api }
LabelPresenter >> emphasis [
	^ emphasis
]

{ #category : #api }
LabelPresenter >> emphasis: anEmphasis [
	"accepts a symbol or an array of symbols: #bold, #italic, #underline, #narrow, #struck"

	emphasis := anEmphasis isSymbol
		ifTrue: [ {anEmphasis} ]
		ifFalse: [ anEmphasis ].
	self changed: #emphasis: with: {emphasis}
]

{ #category : #initialization }
LabelPresenter >> initialize [
	super initialize.
	labelHolder := '' asValueHolder.
	emphasis := #().
	labelHolder whenChangedDo: [ self changed: #getText ].
]

{ #category : #api }
LabelPresenter >> label [
	"<api: #inspect>"

	^ labelHolder value
]

{ #category : #api }
LabelPresenter >> label: aText [
	"<api: #string getter: #label>"
	"Set the text of the label"
	
	labelHolder value: aText
]

{ #category : #'api-valueHolder' }
LabelPresenter >> labelHolder [
	"To register actions to be done when the label changes."
	^ labelHolder
]
