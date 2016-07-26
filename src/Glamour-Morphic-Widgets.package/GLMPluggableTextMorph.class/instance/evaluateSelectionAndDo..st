do-its
evaluateSelectionAndDo: aBlock

	^ self handleEdit: [textMorph editor evaluateSelectionAndDo: aBlock]