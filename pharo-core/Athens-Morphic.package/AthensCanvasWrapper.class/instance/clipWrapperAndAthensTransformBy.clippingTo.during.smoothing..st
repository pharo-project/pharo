private clip transform
clipWrapperAndAthensTransformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize
"the clipTransform methods are just trial and error attemps on getting this right"
	| rect |
	rect := aDisplayTransform globalBoundsToLocal: aClipRect.
	self
		clipBy: rect
		during: [ :c | 
			c canvas pathTransform
				restoreAfter: [ 
					"c canvas morphicClipBy: rect during: [ "
					c canvas pathTransform translateBy: aDisplayTransform offset negated.
					c canvas pathTransform rotateByRadians: aDisplayTransform angle negated.
					c  canvas pathTransform scaleBy: aDisplayTransform scale.
					aBlock value: self ] ]	"]"