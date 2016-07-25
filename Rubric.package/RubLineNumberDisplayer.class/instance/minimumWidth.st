geometry
minimumWidth
	^ (self fontToUse widthOfString: 'MM') + self horizontalGapAfter + self horizontalGapBefore + self  verticalSeparatorWidth 
