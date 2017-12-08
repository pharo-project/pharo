"
A CircleMorphTest is a subclass of MorphTest.  It was first implemented when removing some unused and broken functionality.

My fixtures are morph, a CircleMorph and world.

"
Class {
	#name : #CircleMorphTest,
	#superclass : #MorphTest,
	#category : #'Morphic-Tests-Basic'
}

{ #category : #running }
CircleMorphTest >> setUp [
	super setUp.
	morph := CircleMorph new
]
