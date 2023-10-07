"
A simple ballon morph with straight segments
"
Class {
	#name : 'SimpleBalloonMorph',
	#superclass : 'BalloonMorph',
	#category : 'Morphic-Base-Widgets',
	#package : 'Morphic-Base',
	#tag : 'Widgets'
}

{ #category : 'private' }
SimpleBalloonMorph class >> getVertices: bounds [
	"Construct vertices for a balloon up and to left of anchor."

	^(bounds expandBy: 3) corners atAll: #(1 4 3 2)
]

{ #category : 'initialization' }
SimpleBalloonMorph >> initialize [

	super initialize.
	self beStraightSegments
]
