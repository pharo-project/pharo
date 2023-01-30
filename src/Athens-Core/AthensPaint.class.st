"
I am a base class, which defines a paint.

My subclasses represent a concreate paint by implementing my protocol,  that actually draws a specific shapes with me. My subclasses usually backend-specific, but as long as you implement my protocol, any object can be treated as paint:

 - athensFillPath: aPath on: aCanvas
 - athensFillRectangle: aRectangle on: aCanvas
 - asStrokePaint






"
Class {
	#name : #AthensPaint,
	#superclass : #AthensAbstractPaint,
	#category : #'Athens-Core-Paints'
}

{ #category : #converting }
AthensPaint >> asStrokePaintOn: aCanvas [
	"default implementation"
	^ aCanvas surface createStrokePaintFor: self
]

{ #category : #rendering }
AthensPaint >> athensFillPath: aPath on: aCanvas [

	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	self subclassResponsibility
]

{ #category : #rendering }
AthensPaint >> athensFillRectangle: aRectangle on: aCanvas [
	"This is a terminal method in rendering dispatch scheme
	canvas->shape->paint. See AthensCanvas>>fillShape: "

	self subclassResponsibility
]
