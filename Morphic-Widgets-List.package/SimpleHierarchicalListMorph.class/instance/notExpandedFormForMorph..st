accessing
notExpandedFormForMorph: aMorph
	^ (self selectedMorph = aMorph and: [self theme selectionColor luminance < 0.6])
		ifTrue: [self theme whiteTreeUnexpandedForm]
		ifFalse: [self theme treeUnexpandedForm]
