menu commands
exploreIt
	self handleEdit:
		[textMorph editor evaluateSelectionAndDo: [:result | result inspect]].