"
An InputEventHandler is the abstract superclass for all input event handlers.
Subclasses need to implement handleEvent:.

Instance Variables
	eventFetcher:		<InputEventFetcher>

eventFetcher
	- the event fetcher I'm registered with and receiving my events from.

"
Class {
	#name : #InputEventHandler,
	#superclass : #Object,
	#instVars : [
		'eventFetcher'
	],
	#pools : [
		'EventSensorConstants'
	],
	#category : #'System-VMEvents-Base'
}

{ #category : #'events - testing' }
InputEventHandler class >> isKeyboardEvent: eventBuffer [
	^ eventBuffer first = EventTypeKeyboard and: [ eventBuffer fourth = EventKeyChar ]
]

{ #category : #events }
InputEventHandler >> flushEvents [
]

{ #category : #events }
InputEventHandler >> handleEvent: eventBuffer [
	self subclassResponsibility
]

{ #category : #'events - testing' }
InputEventHandler >> isKeyboardEvent: eventBuffer [

	^self class isKeyboardEvent: eventBuffer
]

{ #category : #initialization }
InputEventHandler >> registerIn: anEventFetcher [
	eventFetcher := anEventFetcher.
	eventFetcher registerHandler: self
]

{ #category : #initialization }
InputEventHandler >> unregister [
	eventFetcher ifNotNil: [
			eventFetcher unregisterHandler: self.
			eventFetcher := nil. ]
]
