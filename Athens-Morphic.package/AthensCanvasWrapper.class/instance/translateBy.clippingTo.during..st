canvas drawing-support
translateBy: offset clippingTo: aRect during: aBlock
	self
		canvasClipBy: aRect
		during: [ 
			self canvas pathTransform
				restoreAfter: [ 
					self canvas pathTransform translateBy: offset.
					aBlock value: self ] ]