Class {
	#name : #GLMHaloBrick,
	#superclass : #HaloMorph,
	#category : #'Glamour-Morphic-Brick-Widgets-UI'
}

{ #category : #handles }
GLMHaloBrick >> addDupHandle: haloSpec [
]

{ #category : #handles }
GLMHaloBrick >> addGrabHandle: haloSpec [
]

{ #category : #updating }
GLMHaloBrick >> changed [

	^ fullBounds 
		ifNil: [ self invalidRect: self privateFullBounds]
		ifNotNil: [ self invalidRect: fullBounds ]
]

{ #category : #initialization }
GLMHaloBrick >> initialize [
	super initialize.
	
	self fillStyle: (Color lightBlue alpha: 0.2)
]

{ #category : #settings }
GLMHaloBrick >> showBoundsInHalo [

	^ true
]
