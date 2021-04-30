"
In Athens, any object can play role of a shape.

I demonstrating the minimal protocol which should be supported
by ""shape"" role, to be used with Athens canvas.
See my methods comments for more details.

"
Class {
	#name : #AthensAbstractShape,
	#superclass : #Object,
	#category : #'Athens-Core-Shapes'
}

{ #category : #testing }
AthensAbstractShape class >> isAbstract [

	^ self == AthensAbstractShape
]

{ #category : #converting }
AthensAbstractShape >> asAthensShapeOn: canvas [
	"Note: The answered object of this message should conform with AthensShape protocol.	"
	self subclassResponsibility
]
