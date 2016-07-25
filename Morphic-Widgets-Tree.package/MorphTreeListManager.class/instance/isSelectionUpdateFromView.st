selection change
isSelectionUpdateFromView
	^ isSelectionUpdateFromView ifNil: [isSelectionUpdateFromView := false] 