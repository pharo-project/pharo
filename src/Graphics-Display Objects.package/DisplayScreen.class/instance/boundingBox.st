other
boundingBox
	clippingBox == nil
		ifTrue: [clippingBox := super boundingBox].
	^ clippingBox