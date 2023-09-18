"
I am an abstract paint, which should be converted before it can be used by Athens.
I am backend agnostic, as opposed to concrete paint(s).
Also, any other object can play the role of paint, as long as it implements the conversion method, #asAthensPaintOn:

See other implementors of #asAthensPaintOn:, such as Color and Form.
"
Class {
	#name : #AthensAbstractPaint,
	#superclass : #Object,
	#category : #'Athens-Core-Paints'
}

{ #category : #testing }
AthensAbstractPaint class >> isAbstract [

	^ self == AthensAbstractPaint
]

{ #category : #converting }
AthensAbstractPaint >> asAthensPaintOn: aCanvas [
	^ self
]
