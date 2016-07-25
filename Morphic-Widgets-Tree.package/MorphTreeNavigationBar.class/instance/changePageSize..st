navigation
changePageSize: aNumberOrNil
	aNumberOrNil
		ifNotNil: [self updateForNewPageSize: aNumberOrNil]
		ifNil: [treeMorph removePager]
