Class {
	#name : #MorphicEventHandlerTest,
	#superclass : #TestCase,
	#instVars : [
		'morph'
	],
	#category : #'Morphic-Tests-Event'
}

{ #category : #running }
MorphicEventHandlerTest >> setUp [
	super setUp.
	morph := Morph new.
	morph eventHandler: MorphicEventHandler new
]

{ #category : #running }
MorphicEventHandlerTest >> tearDown [

	morph := nil.
	super tearDown
]

{ #category : #tests }
MorphicEventHandlerTest >> testClickFromMorph [

	morph eventHandler on: #click send: #value to: true.
	
	self assert: ((morph click: nil) == true)
]

{ #category : #tests }
MorphicEventHandlerTest >> testDoubleClickFromMorph [

	morph eventHandler on: #doubleClick send: #value to: true.
	
	self assert: ((morph doubleClick: nil) == true)
]

{ #category : #tests }
MorphicEventHandlerTest >> testDoubleClickTimeoutFromMorph [

	morph eventHandler on: #doubleClickTimeout send: #value to: true.
	
	self assert: ((morph doubleClickTimeout: nil) == true)
]

{ #category : #tests }
MorphicEventHandlerTest >> testKeyStrokeFromMorph [

	| keyboardEvent |
	keyboardEvent := KeyboardEvent new 
							setType: #keystroke
							buttons: 2
							position: nil
							keyValue: nil
							charCode: 65
							hand: nil
							stamp: nil.

	morph eventHandler on: #keyStroke send: #value to: true.
	
	self assert: ((morph handleKeystroke: keyboardEvent) == true)
]

{ #category : #'tests-events' }
MorphicEventHandlerTest >> testMouseEnterDraggingFromMorph [

	| event | 
	event := MouseEvent basicNew setType: #mouseOver position: nil buttons: 2 hand: Morph new.
	morph eventHandler on: #mouseEnterDragging send: #value to: true.
	
	self assert: ((morph handleMouseEnter: event) == true)
]

{ #category : #'tests-events' }
MorphicEventHandlerTest >> testMouseEnterFromMorph [

	morph eventHandler on: #mouseEnter send: #value to: true.
	
	self assert: ((morph mouseEnter: nil) == true)
]

{ #category : #'tests-events' }
MorphicEventHandlerTest >> testMouseLeaveFromMorph [

	morph eventHandler on: #mouseLeave send: #value to: true.
	
	self assert: ((morph mouseLeave: nil) == true)
]

{ #category : #tests }
MorphicEventHandlerTest >> testTwoEventHandlersAreAttached [

	| keyboardEvent first second |
	keyboardEvent := KeyboardEvent new 
							setType: #keystroke
							buttons: 2
							position: nil
							keyValue: 65
							charCode: 65
							hand: nil
							stamp: nil.

	first := false.
	second := false.
	morph eventHandler on: #keyStroke send: #value to: [ first := true ].
	morph eventHandler on: #keyStroke send: #value to: [ second := true ].
	morph handleKeystroke: keyboardEvent.
	
	self assert: first.
	self assert: second.
]
