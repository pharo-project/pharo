Class {
	#name : #RubTextSegmentIconDisplayer,
	#superclass : #RubScrolledTextSideRuler,
	#instVars : [
		'previouslyIn'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubTextSegmentIconDisplayer class >> key [
	^ #textSegmentIcons
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> backgroundColor [
	^ self paragraphProvider lineNumbersBackgroundColor 
]

{ #category : #geometry }
RubTextSegmentIconDisplayer >> computedWidthFrom: aRectangle [
	^  (self horizontalGapBefore + self itemWidth + self horizontalGapAfter + self verticalSeparatorWidth) truncated 
]

{ #category : #drawing }
RubTextSegmentIconDisplayer >> drawOn: aCanvas [
	| visibleRectangle |
	super drawOn: aCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	aCanvas
		clipBy: self bounds
		during: [ :clippedCanvas | 
			| tl br |
			clippedCanvas fillRectangle: self verticalSeparatorBounds color: self verticalSeparatorColor.
			visibleRectangle := clippedCanvas clipRect.
			tl := self lineIndexForPoint: visibleRectangle topLeft.
			br := self lineIndexForPoint: visibleRectangle bottomRight.
			tl to: br do: [ :i | 
				((self paragraphProvider segmentsAtLine: i) select: [ :s | s icon notNil ])
					ifNotEmpty: [ :segments | 
						| s pos |
						s := segments first.
						s displayIconAt: (pos := self iconPositionOfSegment: s) on: aCanvas.
						segments size > 1
							ifTrue: [ 
								| r ygap |
								ygap := s icon height + 1.
								r := pos + (0 @ ygap) corner: (pos x + 12) @ (pos y + ygap + 2).
								aCanvas fillRectangle: (r expandBy: 2) color: (Color white alpha: 0.4).
								aCanvas
									frameRectangle: r
									width: 2
									colors: {(Color white). (Color black)}
									dashes: {1. 1} ] ] ] ]
]

{ #category : #drawing }
RubTextSegmentIconDisplayer >> drawOnAthensCanvas: anAthensCanvas [
	| visibleRectangle |
	super drawOnAthensCanvas: anAthensCanvas.
	self paragraph selectionStart ifNil: [ ^ self ].
	self paragraph selectionStop ifNil: [ ^ self ].
	self paragraph lines ifNil: [ ^ self ].
	anAthensCanvas
		clipBy: self bounds
		during: [ 
			| tl br |
			anAthensCanvas setShape: self verticalSeparatorBounds.
			anAthensCanvas setPaint: self verticalSeparatorColor.
			anAthensCanvas draw.
			visibleRectangle := anAthensCanvas clipRect.
			tl := self lineIndexForPoint: visibleRectangle topLeft.
			br := self lineIndexForPoint: visibleRectangle bottomRight.
			tl to: br do: [ :i | 
				((self paragraphProvider segmentsAtLine: i) select: [ :s | s icon notNil ])
					ifNotEmpty: [ :segments | 
						| s pos |
						s := segments first.
						pos := self iconPositionOfSegment: s.
						s displayIconAt: pos onAthensCanvas: anAthensCanvas.
						segments size > 1
							ifTrue: [ 
								| r ygap path |
								ygap := s icon height + 1.
								r := pos + (0 @ ygap) corner: (pos x + 12) @ (pos y + ygap + 2).
								anAthensCanvas setShape: (r expandBy: 2).
								anAthensCanvas setPaint: (Color white alpha: 0.4).
								anAthensCanvas draw.
								path := anAthensCanvas
									createPath: [ :builder | 
										builder absolute.
										builder moveTo: r topLeft + (2@0).
										builder lineTo: r topRight + (2@0)].
								(anAthensCanvas setStrokePaint: Color black)
									width: 1;
									dashes: {2.4} offset: 0.
								anAthensCanvas drawShape: path ] ] ] ]
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> fontToUse [
	^ RubAbstractTextArea lineNumbersFont 
]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> handleMouseOver: anEvent [
	| lineIndex |
	super handleMouseOver: anEvent.
	lineIndex := self lineIndexForPoint: anEvent position.
	self mouseEnter: anEvent in: ( (self paragraphProvider segmentsAtLine: lineIndex) select: [ :s | s icon notNil ] ).
	previouslyIn ifNotNil: [
		previouslyIn showMouseHasLeaved.
		previouslyIn := nil ].

	

]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> handlesMouseDown: evt [
	^ true
]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> handlesMouseMove: evt [
	^ true
]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> handlesMouseOver: evt [

	^ true
]

{ #category : #geometry }
RubTextSegmentIconDisplayer >> horizontalGapAfter [
	^ 1
]

{ #category : #geometry }
RubTextSegmentIconDisplayer >> horizontalGapBefore [
	^ 1
]

{ #category : #drawing }
RubTextSegmentIconDisplayer >> iconPositionOfSegment: aSegment [
	^ (self left + self horizontalGapBefore) @ aSegment position y
		translateBy: 0 negated @ (self offset y negated + self bounds top)
]

{ #category : #initialization }
RubTextSegmentIconDisplayer >> initialize [
	super initialize.
	self clipSubmorphs: false
]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> invokeIconMenuOfSegments: aSegmentList event: anEvent [
	| menu |
	aSegmentList ifEmpty: [ ^ self ].
	menu := RubSegmentIconMenuMorph new
		defaultTarget: self;
		yourself.
	aSegmentList
		do: [ :seg | seg addItemToMenu: menu ].
	menu hasItems
		ifFalse: [ ^ self ].
	menu borderWidth: 0.
	menu
		on: #mouseLeave
		send: #value
		to: [ 
			menu delete.
			self changed ].
	menu invokeAt: (self iconPositionOfSegment: aSegmentList first) - (1 @ 4 negated) in: self
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> itemWidth [ 
	^ 16
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> level [
	^ 1
]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> mouseEnter: anEvent [
	super mouseEnter: anEvent
]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> mouseEnter: anEvent in: aSegmentList [
	aSegmentList ifEmpty: [ ^self ].
	previouslyIn = aSegmentList first
		ifTrue: [ ^ self ].
	previouslyIn := aSegmentList first.
	self invokeIconMenuOfSegments: aSegmentList event: anEvent.

]

{ #category : #'event handling' }
RubTextSegmentIconDisplayer >> mouseLeave: anEvent [
	previouslyIn
		ifNotNil: [ 
			previouslyIn showMouseHasLeaved.
			previouslyIn := nil ]
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> textColor [
	^ self textArea lineNumbersTextColor 

]

{ #category : #structure }
RubTextSegmentIconDisplayer >> topRendererOrSelf [
	^self 
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> verticalSeparatorBounds [
	| bnds  |
	bnds := self bounds.
	^ self side = #left
		ifTrue: [ (bnds topRight -  (self verticalSeparatorWidth @ 0)) corner: bnds bottomRight ]
		ifFalse: [ (bnds topLeft corner: bnds bottomLeft + (self verticalSeparatorWidth @ 0)) ]
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> verticalSeparatorColor [
	^ (self textArea backgroundColor contrastingBlackAndWhiteColor ) alpha: 0.3
]

{ #category : #accessing }
RubTextSegmentIconDisplayer >> verticalSeparatorWidth [
	^ 1
]
