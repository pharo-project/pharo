geometry
computedWidthFrom: aRectangle
	^ (((self fontToUse widthOfString: self paragraph numberOfPhysicalLines asString) + self horizontalGapBefore
		+ self horizontalGapAfter + self verticalSeparatorWidth) max: self minimumWidth) truncated