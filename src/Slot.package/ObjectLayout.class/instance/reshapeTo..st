reshaping
reshapeTo: aModification
	^ self class 
		extending: aModification classLayout
		scope: LayoutEmptyScope instance
		host: self host