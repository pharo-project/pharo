expanding-collapsing
expandedFormForMorph: aMorph
	"Answer the form to use for expanded items."
	
	^ ((aMorph selected) and: [self selectionColor luminance < 0.7])
		ifTrue: [self theme whiteTreeExpandedForm]
		ifFalse: [self theme treeExpandedForm]