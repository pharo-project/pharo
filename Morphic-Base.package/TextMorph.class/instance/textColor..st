accessing
textColor: aColor

	color = aColor ifTrue: [^ self].
	color := aColor.
	text addAttribute: (TextColor color: aColor).
	self changed.
