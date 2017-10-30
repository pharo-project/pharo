private
setWidgetLayoutWithNoOffset: widget at: index delta: delta

	widget layoutFrame:
		((0 @ currentProportional corner: 1 @ (currentProportional + delta)) asSpecLayoutFrame 
			topOffset: currentOffset;
			yourself).
				
	currentOffset := 0.
	currentProportional := currentProportional + delta.
	
	lastFractionIndex := index