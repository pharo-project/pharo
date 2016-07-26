accessing text
textColor: newColor
	textColor = newColor ifTrue: [^ self].
	textColor := newColor.
	self addAttribute: (TextColor color: newColor).
	self changed.