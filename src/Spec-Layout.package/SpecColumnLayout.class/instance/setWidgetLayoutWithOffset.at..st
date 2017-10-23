private
setWidgetLayoutWithOffset: widget at: index

	height := widget bottomOffset - widget topOffset.
	
	index = size
		ifTrue: [ 
			widget layoutFrame:
				((0 @ 1 corner: 1 @ 1) asSpecLayoutFrame
					topLeftOffset: widget leftOffset @ height negated;
					bottomRightOffset: widget rightOffset @ 0) ]
		ifFalse: [ 
			widget layoutFrame:
				((0 @ currentProportional corner: 1 @ currentProportional) asSpecLayoutFrame
					topLeftOffset: widget leftOffset @ currentOffset;
					bottomRightOffset: widget rightOffset @ (currentOffset + height)).
			currentOffset := currentOffset + height ]