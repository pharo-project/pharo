Class {
	#name : #SimpleBalloonMorph,
	#superclass : #BalloonMorph,
	#category : #'Morphic-Base-Widgets'
}

{ #category : #private }
SimpleBalloonMorph class >> getVertices: bounds [
	"Construct vertices for a balloon up and to left of anchor."

	^(bounds expandBy: 3) corners atAll: #(1 4 3 2)
]

{ #category : #initialization }
SimpleBalloonMorph >> initialize [
	"Initialize the the receiver."
	
	super initialize.
	self beStraightSegments
]
