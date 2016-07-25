expanding-collapsing
notExpandedFormForMorph: aMorph
	^ (aMorph selected and: [self selectionColor luminance < 0.7])
		ifTrue: [self theme whiteTreeUnexpandedForm]
		ifFalse: [self theme treeUnexpandedForm]
