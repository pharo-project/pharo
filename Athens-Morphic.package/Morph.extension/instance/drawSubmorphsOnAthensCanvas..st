*Athens-Morphic
drawSubmorphsOnAthensCanvas: anAthensCanvas
	self
		drawClippedOnAthensCanvas: anAthensCanvas
		during: [ submorphs reverseDo: [ :m | anAthensCanvas fullDrawMorph: m ] ]