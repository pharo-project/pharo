Extension { #name : 'Canvas' }

{ #category : '*Morphic-Core' }
Canvas >> allocateSavedPatch: extent [

	^ self form allocateForm: extent
]

{ #category : '*Morphic-Core' }
Canvas >> contentsOfArea: aRectangle intoSavedPatch: aForm [

	self contentsOfArea: aRectangle into: aForm
]

{ #category : '*Morphic-Core' }
Canvas >> restoreSavedPatch: aForm at: aPoint [

	self drawImage: aForm at: aPoint
]

{ #category : '*Morphic-Core' }
Canvas >> shadowDrawingCanvasWithExtent: extent shadowColor: shadowColor [

	^ (FormCanvas extent: extent depth: 1) asShadowDrawingCanvas:
		  shadowColor
]
