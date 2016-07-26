editing
stylerStyledInBackground: styledCopyOfText
	next ifNil: [ ^self ].
	text string = styledCopyOfText string
		ifTrue: [ self stylerStyled: styledCopyOfText ].
