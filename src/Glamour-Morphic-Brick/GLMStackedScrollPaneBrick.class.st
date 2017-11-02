"
A GLMStackedScrollPaneBrick is a more complicated scrollpane, with scrollbars and stacking mechanism of subbricks that have #stack floating parameter

"
Class {
	#name : #GLMStackedScrollPaneBrick,
	#superclass : #GLMScrollPaneBrick,
	#instVars : [
		'allowedBounds'
	],
	#category : 'Glamour-Morphic-Brick-Widgets-Scrollpane'
}

{ #category : #examples }
GLMStackedScrollPaneBrick class >> example [

	super example
]

{ #category : #accessing }
GLMStackedScrollPaneBrick >> allowedBounds [

	^ allowedBounds ifNil: [ allowedBounds := self globalBounds ]
]

{ #category : #accessing }
GLMStackedScrollPaneBrick >> allowedBounds: aRectangle [

	allowedBounds := aRectangle
]

{ #category : #initialization }
GLMStackedScrollPaneBrick >> initialize [
	super initialize.
	
	self band useVerticalLinearLayout
]

{ #category : #scrolling }
GLMStackedScrollPaneBrick >> invalidate [

	
]

{ #category : #scrolling }
GLMStackedScrollPaneBrick >> update [
	super update.
	
	"we need to update band's layout to support stacking"
	"self allowedBounds: self band layoutChildren."
]
