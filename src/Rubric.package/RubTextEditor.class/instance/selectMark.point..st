new selection
selectMark: mark point: point
	"Select the specified characters inclusive.
	 Be sure the selection is in view."

	(mark =  self markIndex and: [point + 1 = self pointIndex]) ifFalse:
		[self deselect.
		self selectInvisiblyMark: mark point: point]