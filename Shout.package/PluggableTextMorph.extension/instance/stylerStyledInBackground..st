*Shout
stylerStyledInBackground: styledCopyOfText 
	textMorph contents string = styledCopyOfText string
		ifTrue: [self stylerStyled: styledCopyOfText]