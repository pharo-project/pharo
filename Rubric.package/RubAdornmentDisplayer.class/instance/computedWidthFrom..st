accessing
computedWidthFrom: aRectangle
	^  (super computedWidthFrom: aRectangle) min: self computedSize
