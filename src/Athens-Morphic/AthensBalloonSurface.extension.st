Extension { #name : #AthensBalloonSurface }

{ #category : #'*Athens-Morphic' }
AthensBalloonSurface >> displayOnMorphicCanvas: canvas at: aPoint [
	"Display receiver on morphic canvas"

	canvas
		image: self asForm
		at: aPoint
		sourceRect: (0 @ 0 extent: self asForm extent)
		rule: 34
]
