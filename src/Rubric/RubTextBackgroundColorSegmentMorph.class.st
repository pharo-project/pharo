"
I am a segmentmorph intended to implement the visual presentation of a TextBackgroundColor for a text.
"
Class {
	#name : #RubTextBackgroundColorSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#category : #'Rubric-Editing-Core'
}

{ #category : #'as yet unclassified' }
RubTextBackgroundColorSegmentMorph class >> color: aColor from: start to: stop [ 
	^ self new
		color: aColor;
		from: start to: stop; 
		yourself
]

{ #category : #accessing }
RubTextBackgroundColorSegmentMorph >> color: aColor [
	color := aColor
]
