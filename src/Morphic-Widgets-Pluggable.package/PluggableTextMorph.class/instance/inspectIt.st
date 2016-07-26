menu commands
inspectIt
	self handleEdit:
		[textMorph editor evaluateSelectionAndDo: [:result | result inspect]]