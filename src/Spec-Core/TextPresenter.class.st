"
I handle basic multi-line text.
See AbstractWidgetPresenter

self example

You can also have a look at ListSelectionPresenter and ScrollSyncExample for more examples.

My main purpose is to handle source code (I have editing shortcuts support).

I provide the following variables and their accessors
- autoAccept is a boolean representing if the text is accepted on each keystroke.
- text is the text of the text zone.

I provide the following methods
- beForCode and beForText are shortcut for configuring me.
- getSelection return the interval currently selected.
- getText is a getter of the text
"
Class {
	#name : #TextPresenter,
	#superclass : #AbstractTextPresenter,
	#traits : 'TSpecHaveWrappingScrollBars',
	#classTraits : 'TSpecHaveWrappingScrollBars classTrait',
	#instVars : [
		'#scrollValue => SpecObservableSlot'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
TextPresenter class >> adapterName [

	^ #TextAdapter
]

{ #category : #specs }
TextPresenter class >> title [

	^ 'Text'
]

{ #category : #initialization }
TextPresenter >> initialize [ 

	super initialize.
	self flag: #REVIEW. "We really need scrollValue?"
	scrollValue := 0 @ 0.	
	self withScrollBars
]

{ #category : #initialization }
TextPresenter >> registerEvents [

	super registerEvents.
	self whenScrollValueChangedDo: [ :newPosition | 
		self changed: #setScrollValue: with: { newPosition } ]
]

{ #category : #api }
TextPresenter >> scrollValue [
	"<api: #inspect>"
	"Return the current scroll position"

	^ scrollValue
]

{ #category : #api }
TextPresenter >> scrollValue: aPoint [
	"Set the scroll position"

	^ scrollValue := aPoint
]

{ #category : #'api-events' }
TextPresenter >> whenScrollValueChangedDo: aBlock [
	"Set a block to perform when the scroll value changed"
	
	self property: #scrollValue whenChangedDo: aBlock
]
