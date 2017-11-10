"
I am holding a set of mouse buttons state (pressed or not).
Used by various mouse events.
"
Class {
	#name : #OSButtonState,
	#superclass : #Object,
	#instVars : [
		'buttons'
	],
	#category : #OSWindow-Core-Events
}

{ #category : #accessing }
OSButtonState >> anyPressed [
	^ buttons isZero not
]

{ #category : #accessing }
OSButtonState >> button1 [
	^ (buttons & 1) ~=0
]

{ #category : #accessing }
OSButtonState >> button1: aBoolean [
	self setBit: 1 value: aBoolean
]

{ #category : #accessing }
OSButtonState >> button2 [
	^ (buttons & 2) ~=0
]

{ #category : #accessing }
OSButtonState >> button2: aBoolean [
	self setBit: 2 value: aBoolean
]

{ #category : #accessing }
OSButtonState >> button3 [
	^ (buttons & 4) ~=0
]

{ #category : #accessing }
OSButtonState >> button3: aBoolean [
	self setBit: 4 value: aBoolean
]

{ #category : #accessing }
OSButtonState >> button4 [
	^ (buttons & 8) ~=0
]

{ #category : #accessing }
OSButtonState >> button4: aBoolean [
	self setBit: 8 value: aBoolean
]

{ #category : #accessing }
OSButtonState >> button5 [
	^ (buttons & 16) ~=0
]

{ #category : #accessing }
OSButtonState >> button5: aBoolean [
	self setBit: 16 value: aBoolean
]

{ #category : #initialization }
OSButtonState >> initialize [
	buttons := 0
]

{ #category : #private }
OSButtonState >> setBit: bit value: aBoolean [
	
	buttons := buttons - (buttons bitAnd: bit) + (aBoolean ifTrue: [ bit  ] ifFalse: [ 0 ])
]
