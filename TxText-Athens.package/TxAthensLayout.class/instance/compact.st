private
compact
	"Remove lines outside of view, answer updated view's vertical offset"
	| viewOffsetY line height linesToKeep oldLines |
	viewOffsetY := view offset y.
	
	[ line := lines first. viewOffsetY > line height ] whileTrue: [ 
		lines removeFirst notifyRemovedFrom: self. 
		viewOffsetY := viewOffsetY - line height.
	].
	
	height := 0.
	linesToKeep := lines count: [ :each |
		height := height + each height.
		height - viewOffsetY < view extent y
		 ].
	linesToKeep := (linesToKeep +1) min: lines size.
	oldLines := lines allButFirst: linesToKeep.
	lines := lines first: linesToKeep.
	oldLines do: [ :each | each notifyRemovedFrom: self ].

	self updateExtent.

	^ viewOffsetY
