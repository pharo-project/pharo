Extension { #name : 'ShadowDrawingCanvas' }

{ #category : '*Morphic-Core' }
ShadowDrawingCanvas >> paintImageFromScalingCanvas: aScalingCanvas at: aPoint [

	myCanvas formCanvas asShadowDrawingCanvas
		paintImage: aScalingCanvas form
		at: aPoint * aScalingCanvas scale
]
