actions
expandedFormForMorph: aMorph
	"Answer the form to use for expanded items."
	
	^ (self selectedMorph = aMorph and: [self theme selectionColor luminance < 0.6])
		ifTrue: [self theme whiteTreeExpandedForm]
		ifFalse: [self theme treeExpandedForm]