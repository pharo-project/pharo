Extension { #name : 'FormCanvas' }

{ #category : '*Morphic-Core' }
FormCanvas >> allocateCanvas: extent [

	^ (self allocateForm: extent) getCanvas
]

{ #category : '*Morphic-Core' }
FormCanvas >> drawImageOn: aCanvas at: aPoint [

	aCanvas drawImage: self form at: aPoint sourceRect: self form boundingBox
]

{ #category : '*Morphic-Core' }
FormCanvas >> numberOfTransparentPixelsForRoundedCorners [

	^ 48
]

{ #category : '*Morphic-Core' }
FormCanvas >> paintImageOn: aCanvas at: aPoint [

	aCanvas paintImage: self form at: aPoint
]
