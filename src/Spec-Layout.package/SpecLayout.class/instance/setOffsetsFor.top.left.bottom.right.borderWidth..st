private
setOffsetsFor: widget top: top left: left bottom: bottom right: right borderWidth: windowBorderWidth

	self setRightOffset: right for: windowBorderWidth borderWidth: widget.
	self setLeftOffset: left for: widget borderWidth: windowBorderWidth.
	self setBottomOffset: bottom for: widget borderWidth: windowBorderWidth.
	self setTopOffset: top for: widget borderWidth: windowBorderWidth