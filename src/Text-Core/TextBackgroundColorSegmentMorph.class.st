"
I am a segmentmorph intended to implement the visual presentation of a TextBackgroundColor for a text.
"
Class {
	#name : #TextBackgroundColorSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#category : #'Text-Core-Utilities'
}

{ #category : #'as yet unclassified' }
TextBackgroundColorSegmentMorph class >> color: aColor from: start to: stop [ 
	^ self new
		color: aColor;
		from: start to: stop; 
		yourself
]

{ #category : #accessing }
TextBackgroundColorSegmentMorph >> color: aColor [
	color := aColor
]
