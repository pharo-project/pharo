scrollbar managing
scrollBy: delta
	self
		handleScrollerOffsetChange: [ 
			| newYoffset newXoffset |
			"Move the contents in the direction delta."
			newYoffset := scroller offset y - delta y max: 0.
			newXoffset := scroller offset x - delta x max: 0.	"Set the offset on the scroller"
			scroller offset: newXoffset @ newYoffset.
			self updateScrollbars ]