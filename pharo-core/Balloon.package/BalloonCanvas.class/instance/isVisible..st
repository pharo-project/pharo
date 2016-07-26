testing
isVisible: aRectangle
	^transform 
		ifNil:[super isVisible: aRectangle]
		ifNotNil:[super isVisible: (transform localBoundsToGlobal: aRectangle)]