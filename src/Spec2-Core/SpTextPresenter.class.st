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
	#name : #SpTextPresenter,
	#superclass : #SpAbstractTextPresenter,
	#traits : 'SpTHaveWrappingScrollBars',
	#classTraits : 'SpTHaveWrappingScrollBars classTrait',
	#instVars : [
		'#scrollValue => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Text'
}

{ #category : #specs }
SpTextPresenter class >> adapterName [

	^ #TextAdapter
]

{ #category : #specs }
SpTextPresenter class >> title [

	^ 'Text'
]

{ #category : #initialization }
SpTextPresenter >> initialize [ 

	super initialize.
	self flag: #REVIEW. "We really need scrollValue?"
	scrollValue := 0 @ 0.	
	self withScrollBars
]

{ #category : #initialization }
SpTextPresenter >> registerEvents [

	super registerEvents.
	self whenScrollValueChangedDo: [ :newPosition | 
		self changed: #setScrollValue: with: { newPosition } ]
]

{ #category : #api }
SpTextPresenter >> scrollValue [
	"Return the current scroll position"

	^ scrollValue
]

{ #category : #api }
SpTextPresenter >> scrollValue: aPoint [
	"Set the scroll position"

	^ scrollValue := aPoint
]

{ #category : #'api-events' }
SpTextPresenter >> whenScrollValueChangedDo: aBlock [
	"Set a block to perform when the scroll value changed"
	
	self property: #scrollValue whenChangedDo: aBlock
]
