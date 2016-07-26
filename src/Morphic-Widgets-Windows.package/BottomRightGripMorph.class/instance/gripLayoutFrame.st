accessing
gripLayoutFrame
	^ (1 @ 1 corner: 1 @ 1) asLayoutFrame 
		topLeftOffset: (0 - self defaultWidth @ (0 - self defaultHeight))
		
	