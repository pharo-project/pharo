Class {
	#name : 'MouseClickStateTest',
	#superclass : 'TestCase',
	#instVars : [
		'morph',
		'hand',
		'event'
	],
	#category : 'Morphic-Tests-Event',
	#package : 'Morphic-Tests',
	#tag : 'Event'
}

{ #category : 'running' }
MouseClickStateTest >> setUp [
	super setUp.
	morph := MockMorph new.
	morph eventHandler: MorphicEventHandler new.
	hand := HandMorph new
		mouseFocus: morph;
		yourself.
	hand privateOwner: morph.
	event := MouseButtonEvent new
		setType: #mouseDown
		position: 0@0
		which: 0
		buttons: 4
		hand: hand
		stamp: Time millisecondClockValue
]

{ #category : 'running' }
MouseClickStateTest >> tearDown [
	morph delete.
	morph := event := hand := nil.
	super tearDown
]

{ #category : 'tests' }
MouseClickStateTest >> testClickFromMorph [
	| mcs |
	mcs := MouseClickState new.
	self assert: mcs clickState equals: #firstClickDown.
	#(clickSelector dragSelector doubleClickSelector) do: [ :selector |
		self assert: (selector value: mcs) isNil
		 ]
]

{ #category : 'tests' }
MouseClickStateTest >> testDoubleClickShouldNotTriggerIfClickedOutsideBounds [
	(hand mouseClickStateFor: morph event: event)
		doubleClickSelector: #myMagicDoubleClick:.
	"the first click does not do anything"
	hand handleEvent: event.
	event setType: #mouseUp.
	hand handleEvent: event.

	"Out of bounds"
	event setPosition: morph bounds topRight + 100.
	event setType: #mouseDown.
	hand handleEvent: event.

	self deny: morph hasMyMagicDoubleClickActivated
]

{ #category : 'tests' }
MouseClickStateTest >> testHandleEventFrom [
	hand
		waitForClicksOrDrag: morph
		event: event
		"template"
		"{#click: #doubleClick: #doubleClickTimeout: #startDrag:}"
		selectors: {#myMagicClick:. nil. nil. nil}
		threshold: 10.
	"please use:
	(hand mouseClickStateFor: morph event: event)
		clickSelector: #myMagicClick:.
	"
	"the first click does not do anything"
	hand handleEvent: event.
	"the mouse up triggers myMagicClick: "
	event setType: #mouseUp.
	hand handleEvent: event.

	self assert: morph hasMyMagicClickActivated
]

{ #category : 'tests' }
MouseClickStateTest >> testHandleEventFromDragDoubleClick [
	(hand mouseClickStateFor: morph event: event)
		doubleClickSelector: #myMagicDoubleClick:.
	"the first click does not do anything"
	hand handleEvent: event.
	event setType: #mouseUp.
	hand handleEvent: event.

	event setType: #mouseDown.
	hand handleEvent: event.

	self assert: morph hasMyMagicDoubleClickActivated
]
