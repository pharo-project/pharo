geometry
setScrollDeltas
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."

	scroller hasSubmorphs ifFalse: 
		[scrollBar interval: 1.0. 
		hScrollBar interval: 1.0. 
		^ self].
	
"NOTE: fullbounds commented out now -- trying to find a case where this expensive step is necessary -- perhaps there is a less expensive way to handle that case."
	"scroller fullBounds." "force recompute so that leftoverScrollRange will be up-to-date"
	self hideOrShowScrollBars.
	
	self vIsScrollbarShowing ifTrue:[ self vSetScrollDelta ].
	self hIsScrollbarShowing ifTrue:[ self hSetScrollDelta ].
