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
	self withScrollBars
]
