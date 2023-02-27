Extension { #name : #AthensCairoSurface }

{ #category : #'*Athens-Morphic' }
AthensCairoSurface >> displayOnMorphicCanvas: canvas at: aPoint [

	canvas
		image: self asForm
		at: aPoint
		sourceRect: (0@0 extent: self extent )
		rule: 34
]
