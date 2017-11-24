"
I am an abstract class that knows my bounds and if a point is contained in me.
"
Class {
	#name : #Shape,
	#superclass : #Object,
	#category : #'Graphics-Shapes'
}

{ #category : #testing }
Shape >> basicContainsPoint: aPoint [
	"Answer whether the receiver contains the given point."

	^self bounds containsPoint: aPoint
]

{ #category : #accessing }
Shape >> bounds [
	"Answer a rectangle that encloses the receiver."

	self subclassResponsibility
]

{ #category : #testing }
Shape >> containsPoint: aPoint [
	"Answer whether the receiver contains the given point."

	^self basicContainsPoint: aPoint
]
