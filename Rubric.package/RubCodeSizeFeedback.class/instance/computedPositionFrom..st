accessing
computedPositionFrom: aRectangle
	^  ((aRectangle right min: self scrollBounds right) - (self computedWidthFrom: aRectangle)) @ aRectangle top
