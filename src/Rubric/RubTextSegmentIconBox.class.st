Class {
	#name : #RubTextSegmentIconBox,
	#superclass : #Morph,
	#instVars : [
		'previouslyIn'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubTextSegmentIconBox class >> key [
	^ #textSegmentIcons
]

{ #category : #accessing }
RubTextSegmentIconBox >> backgroundColor [
	^ Color orange veryMuchLighter
]

{ #category : #'event handling' }
RubTextSegmentIconBox >> handlesMouseMove: evt [
	^ true
]

{ #category : #'event handling' }
RubTextSegmentIconBox >> mouseEnter: anEvent [
	self color: (Color lightGray alpha: 0.04).

]

{ #category : #'event handling' }
RubTextSegmentIconBox >> mouseEnter: anEvent in: aSegment [
	previouslyIn = aSegment
		ifTrue: [ ^ self ].
	previouslyIn := aSegment.
	aSegment showMouseHasEntered 
]

{ #category : #'event handling' }
RubTextSegmentIconBox >> mouseLeave: anEvent [
	self color: self defaultBackgroundColor.
	previouslyIn
		ifNotNil: [ 
			previouslyIn showMouseHasLeaved.
			previouslyIn := nil ]
]

{ #category : #accessing }
RubTextSegmentIconBox >> textColor [
	^ self textArea lineNumbersTextColor 

]

{ #category : #accessing }
RubTextSegmentIconBox >> verticalSeparatorBounds [
	| bnds  |
	bnds := self bounds.
	^ self side = #left
		ifTrue: [ (bnds topRight -  (self verticalSeparatorWidth @ 0)) corner: bnds bottomRight ]
		ifFalse: [ (bnds topLeft corner: bnds bottomLeft + (self verticalSeparatorWidth @ 0)) ]
]

{ #category : #accessing }
RubTextSegmentIconBox >> verticalSeparatorColor [
	^ (self textArea backgroundColor contrastingBlackAndWhiteColor ) alpha: 0.3
]

{ #category : #accessing }
RubTextSegmentIconBox >> verticalSeparatorWidth [
	^ 1
]
