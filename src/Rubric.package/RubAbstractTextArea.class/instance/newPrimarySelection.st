accessing selection
newPrimarySelection
	^ RubPrimarySelectionMorph inTextArea: self from: self markIndex to: self pointIndex.
