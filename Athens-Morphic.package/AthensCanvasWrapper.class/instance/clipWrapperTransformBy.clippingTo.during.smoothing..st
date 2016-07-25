private clip transform
clipWrapperTransformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize
"the clipTransform methods are just trial and error attemps on getting this right"
	| rect |
	rect := aDisplayTransform globalBoundsToLocal: aClipRect.
	self canvas pathTransform
		restoreAfter: [ 
			self canvas pathTransform translateBy: aDisplayTransform offset negated.
			self canvas pathTransform rotateByRadians: aDisplayTransform angle negated.
			self canvas pathTransform scaleBy: aDisplayTransform scale.
			self clipBy: rect during: [ :c | aBlock value: c ]]

		