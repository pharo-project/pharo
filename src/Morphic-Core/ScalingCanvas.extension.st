Extension { #name : 'ScalingCanvas' }

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
ScalingCanvas >> restoreSavedPatch: savedPatch at: aPoint [

	formCanvas restoreSavedPatch: savedPatch form at: aPoint * scale
]
