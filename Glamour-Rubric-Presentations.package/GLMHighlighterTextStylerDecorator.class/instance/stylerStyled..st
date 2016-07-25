editing
stylerStyled: styledCopyOfText
	next ifNil: [ ^self ].
	text runs: styledCopyOfText runs.
	(self textArea scrollPane ifNil: [ self textArea ]) changed