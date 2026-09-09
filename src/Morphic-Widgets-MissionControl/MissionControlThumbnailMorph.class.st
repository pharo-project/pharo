Class {
	#name : 'MissionControlThumbnailMorph',
	#superclass : 'BorderedMorph',
	#instVars : [
		'window',
		'thumbnailForm',
		'labelMorph',
		'isHighlighted',
		'originalBounds'
	],
	#category : 'Morphic-Widgets-MissionControl',
	#package : 'Morphic-Widgets-MissionControl'
}

{ #category : 'drawing' }
MissionControlThumbnailMorph >> baseBorderColor [

	^ self theme baseBorderColor
]

{ #category : 'private' }
MissionControlThumbnailMorph >> buildLabel [

	| title |

	labelMorph ifNotNil: [ ^ labelMorph ].
	MissionControlSettings showWindowLabels ifFalse: [ ^ nil ].
	
	title := window labelString.
	title size > self maxTtleSize ifTrue: [
		title := (title first: self maxTtleSize) , '...'
	].

	labelMorph := StringMorph contents: title font: StandardFonts defaultFont.
	labelMorph color: Color white.

	^ labelMorph
]

{ #category : 'drawing' }
MissionControlThumbnailMorph >> drawOn: aCanvas [

	super drawOn: aCanvas.
	thumbnailForm ifNotNil: [
		aCanvas translucentImage: thumbnailForm at: self position + (self width - thumbnailForm width // 2 @ 2) ].
	
	borderColor := isHighlighted ifTrue: [ self highlightBorderColor ] ifFalse: [ self baseBorderColor ].
	borderWidth := isHighlighted ifTrue: [ 4 ] ifFalse: [ 2 ].
	
	self borderColor: borderColor.
	aCanvas frameRectangle: self bounds width: borderWidth color: borderColor
]

{ #category : 'drawing' }
MissionControlThumbnailMorph >> highlightBorderColor [

	^ self theme selectedTabBorderColor
]

{ #category : 'initialization' }
MissionControlThumbnailMorph >> initializeForWindow: aSystemWindow [

	self initializeForWindow: aSystemWindow extent: (aSystemWindow extent * 0.25) rounded
]

{ #category : 'initialization' }
MissionControlThumbnailMorph >> initializeForWindow: aSystemWindow extent: thumbExtent [

	| safeExtent |

	window := aSystemWindow.
	originalBounds := aSystemWindow bounds.
	safeExtent := thumbExtent max: 10 @ 10.
	thumbnailForm := self safeThumbnailOf: aSystemWindow size: safeExtent.
	isHighlighted := false.

	self extent: safeExtent.
	self borderColor: self baseBorderColor.
	self color: Color white.
	self borderWidth: 2.
	self useRoundedCorners.
	self
		on: #mouseEnter send: #mouseEnter:event: to: self;
		on: #mouseLeave send: #mouseLeave:event: to: self
]

{ #category : 'private' }
MissionControlThumbnailMorph >> labelHeight [

	^ MissionControlSettings showWindowLabels
		  ifTrue: [ StandardFonts defaultFont height + 4 ]
		  ifFalse: [ 0 ]
]

{ #category : 'accessing' }
MissionControlThumbnailMorph >> labelMorph [

	^ labelMorph
]

{ #category : 'private' }
MissionControlThumbnailMorph >> maxTtleSize [

	^ 30
]

{ #category : 'event handling' }
MissionControlThumbnailMorph >> mouseEnter: evt event: aMorph [

	isHighlighted := true.
	self changed
]

{ #category : 'event handling' }
MissionControlThumbnailMorph >> mouseLeave: evt event: aMorph [

	isHighlighted := false.
	self changed
]

{ #category : 'private' }
MissionControlThumbnailMorph >> safeThumbnailOf: aWindow size: extent [

	^ [ aWindow asFormOfSize: extent ] on: Error do: [ Form extent: extent depth: 32 ]
]

{ #category : 'accessing' }
MissionControlThumbnailMorph >> window [

	^ window
]
