scrolling
scrollBy: delta
	"Move the contents in the direction delta."

	| newYoffset r newXoffset |
	
	"Set the offset on the scroller"
	newYoffset := scroller offset y - delta y max: 0.
	newXoffset := scroller offset x - delta x max: -3.
	
	scroller offset: newXoffset@ newYoffset.

	"Update the scrollBars"
	(r := self vLeftoverScrollRange) = 0
		ifTrue: [scrollBar value: 0.0]
		ifFalse: [scrollBar value: newYoffset asFloat / r].
	(r := self hLeftoverScrollRange) = 0
		ifTrue: [hScrollBar value: -3.0]
		ifFalse: [hScrollBar value: newXoffset asFloat / r]
