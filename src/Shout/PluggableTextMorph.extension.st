Extension { #name : #PluggableTextMorph }

{ #category : #'*Shout' }
PluggableTextMorph >> stylerStyled: styledCopyOfText [ 

	textMorph contents runs: styledCopyOfText runs.
	textMorph updateFromParagraph.
	selectionInterval
		ifNotNil: [textMorph editor selectInvisiblyFrom: selectionInterval first to: selectionInterval last;
				 storeSelectionInParagraph;
				 setEmphasisHere].
	textMorph editor blinkParen.
	self scrollSelectionIntoView
]

{ #category : #'*Shout' }
PluggableTextMorph >> stylerStyledInBackground: styledCopyOfText [ 
	textMorph contents string = styledCopyOfText string
		ifTrue: [self stylerStyled: styledCopyOfText]
]
