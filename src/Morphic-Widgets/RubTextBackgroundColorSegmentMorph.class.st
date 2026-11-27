"
I am a segmentmorph intended to implement the visual presentation of a TextBackgroundColor for a text.
"
Class {
	#name : 'RubTextBackgroundColorSegmentMorph',
	#superclass : 'RubTextSegmentMorph',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'instance creation' }
RubTextBackgroundColorSegmentMorph class >> color: aColor from: start to: stop [
	^ self new
		color: aColor;
		from: start to: stop;
		yourself
]

{ #category : 'accessing' }
RubTextBackgroundColorSegmentMorph >> color: aColor [
	color := aColor
]
