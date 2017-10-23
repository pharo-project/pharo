private
setWidgetLayoutWithOffset: e at: idx
	| y0 y1 |
	
	width := e offsets width.
	y0 := e offsets top.
	y1 := e offsets bottom.
	
	idx = size
		ifTrue: [ 
			e layoutFrame:
				((1 @ 0 corner: 1 @ 1) asSpecLayoutFrame
					topLeftOffset: width negated @ y0;
					bottomRightOffset: 0 @ y1) ]
		ifFalse: [ 
			e layoutFrame:
				((currentProportional @ 0 corner: currentProportional @ 1) asSpecLayoutFrame
					topLeftOffset: currentOffset @ y0;
					bottomRightOffset: (currentOffset + width) @ y1).
			currentOffset := currentOffset + width ]