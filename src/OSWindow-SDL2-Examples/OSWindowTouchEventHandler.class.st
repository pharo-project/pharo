"
I am the basic event handler for the example
"
Class {
	#name : #OSWindowTouchEventHandler,
	#superclass : #OSWindowEventVisitor,
	#instVars : [
		'touchExample'
	],
	#category : 'OSWindow-SDL2-Examples-Touch'
}

{ #category : #api }
OSWindowTouchEventHandler >> touchExample:	renderer [
		touchExample := renderer
]

{ #category : #visiting }
OSWindowTouchEventHandler >> visitTouchPointerDownEvent: anEvent [
	touchExample beginMove: anEvent fingerId at: anEvent position.
]

{ #category : #visiting }
OSWindowTouchEventHandler >> visitTouchPointerMoveEvent: anEvent [
	touchExample move: anEvent fingerId at: anEvent position
]

{ #category : #visiting }
OSWindowTouchEventHandler >> visitTouchPointerUpEvent: anEvent [
	touchExample endMove: anEvent fingerId at: anEvent position
]

{ #category : #visiting }
OSWindowTouchEventHandler >> visitWindowCloseEvent: anEvent [
	touchExample destroy
]
