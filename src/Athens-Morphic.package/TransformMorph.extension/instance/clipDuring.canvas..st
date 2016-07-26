*Athens-Morphic
clipDuring: aBlock canvas: aCanvas 

	self clipSubmorphs 
		ifTrue: [ 
			aCanvas
				clipBy: self clippingBounds 
				during: aBlock ] 
		ifFalse: [ ^ aBlock value ]
