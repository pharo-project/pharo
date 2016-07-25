new selection
selectInvisiblyMark: mark point: point
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: mark pointIndex: point + 1