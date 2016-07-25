*Shout
stylerStyled: styledCopyOfText 

	textMorph contents runs: styledCopyOfText runs.
	textMorph updateFromParagraph.
	selectionInterval
		ifNotNil: [textMorph editor selectInvisiblyFrom: selectionInterval first to: selectionInterval last;
				 storeSelectionInParagraph;
				 setEmphasisHere].
	textMorph editor blinkParen.
	self scrollSelectionIntoView