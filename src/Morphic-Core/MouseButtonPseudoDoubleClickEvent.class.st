"
I get created from a mouseDown MouseButtonEvent, adding the second mouseDown event by cloning the first event, and repositioning it to the current hand location.
I can be usefull for refining some doubleClick: implementations. (as is done for FTTableMorph>>#doubleClick:)
"
Class {
	#name : 'MouseButtonPseudoDoubleClickEvent',
	#superclass : 'MouseButtonEvent',
	#instVars : [
		'secondEvent'
	],
	#category : 'Morphic-Core-Events',
	#package : 'Morphic-Core',
	#tag : 'Events'
}

{ #category : 'accessing' }
MouseButtonPseudoDoubleClickEvent >> secondEvent [

	^ secondEvent
]

{ #category : 'accessing' }
MouseButtonPseudoDoubleClickEvent >> secondEvent: anObject [

	secondEvent := anObject
]
