"
i am the root of various mouse events.

Note, that my subclasses care to represent only the most basic events, not the synthethic ones, like click or double-click, which are result of interpreting the sequence of raw events.
"
Class {
	#name : #OSMouseEvent,
	#superclass : #OSEvent,
	#instVars : [
		'position',
		'delta',
		'modifiers'
	],
	#category : #OSWindow-Core-Events
}

{ #category : #accessing }
OSMouseEvent >> buttons [
	^ modifiers buttons
]

{ #category : #accessing }
OSMouseEvent >> delta [
	
	^ delta
]

{ #category : #accessing }
OSMouseEvent >> delta: anObject [
	
	delta := anObject
]

{ #category : #initialization }
OSMouseEvent >> initialize [
	super initialize.
	modifiers := OSStateModifiers new
]

{ #category : #accessing }
OSMouseEvent >> modifiers [
	^ modifiers 
]

{ #category : #accessing }
OSMouseEvent >> position [
	
	^ position
]

{ #category : #accessing }
OSMouseEvent >> position: anObject [
	
	position := anObject
]
