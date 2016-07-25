private
computeNotSplitterWidget: widget
	
	self
		setOffsetsFor: widget
		top: widget topOffset
		left: widget leftOffset
		bottom: widget bottomOffset
		right: widget rightOffset
		borderWidth: self class windowBorderWidth.
	
	widget generateArguments.
	widget asSpecElements do: [ :el | result add: el ]