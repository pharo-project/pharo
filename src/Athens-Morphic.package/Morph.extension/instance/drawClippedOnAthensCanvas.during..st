*Athens-Morphic
drawClippedOnAthensCanvas: anAthensCanvas during: aBlock

	^ self clipSubmorphs 
		ifTrue: [ 
			 anAthensCanvas
				clipBy: (self clippingBounds intersect: anAthensCanvas clipRect ifNone:[^ self])
				during: aBlock ] 
		ifFalse: [ aBlock value ]
