menu commands
basicInspectIt
	self handleEdit:
		[textMorph editor evaluateSelectionAndDo: [:result | result basicInspect]].