accessing
lineColor
	"Answer a good color to use for drawing the lines that connect members of the hierarchy view.
	Used the cached color, or derive it if necessary by finding the receiver or 
	the first owner (up to my root) that is not transparent, then picking a contrasting color.
	Fall back to black if all my owners are transparent."

	^ lineColor 
		ifNil: [lineColor := container lineColorForNode: self complexContents]
	
