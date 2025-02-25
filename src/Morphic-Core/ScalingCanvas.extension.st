Extension { #name : 'ScalingCanvas' }

{ #category : '*Morphic-Core' }
ScalingCanvas >> allocateCanvas: extent [

	^ self class formCanvas: (formCanvas allocateCanvas: extent * scale) scale: scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> allocateSavedPatch: extent [

	^ ScalingCanvasSavedPatch
		form: (formCanvas allocateForm: extent * scale)
		scale: scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> contentsOfArea: aRectangle intoSavedPatch: savedPatch [

	formCanvas contentsOfArea: (aRectangle scaleBy: scale) intoSavedPatch: savedPatch form
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> drawImageOn: aCanvas at: aPoint [

	aCanvas formCanvas drawImage: self form
		at: aPoint * scale
		sourceRect: (self form boundingBox scaleBy: scale)
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> numberOfTransparentPixelsForRoundedCorners [

	^ formCanvas numberOfTransparentPixelsForRoundedCorners * scale * scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> paintImageFromScalingCanvas: aScalingCanvas at: aPoint [

	formCanvas paintImage: aScalingCanvas form at: aPoint * scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> paintImageOn: aCanvas at: aPoint [

	aCanvas paintImageFromScalingCanvas: self at: aPoint
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> restoreSavedPatch: savedPatch at: aPoint [

	formCanvas restoreSavedPatch: savedPatch form at: aPoint * scale
]

{ #category : '*Morphic-Core' }
ScalingCanvas >> shadowDrawingCanvasWithExtent: extent shadowColor: shadowColor [

	^ (self class formCanvas: (FormCanvas on: (Form extent: extent * scale depth: 1)) scale: scale)
		asShadowDrawingCanvas: shadowColor
]
