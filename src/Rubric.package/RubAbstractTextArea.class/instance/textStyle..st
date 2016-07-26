accessing text
textStyle: aTextStyle
	self privateTextStyle: aTextStyle.
	self addAttribute: (TextFontChange fontNumber: (textStyle defaultFontIndex)).
	self paragraph compose.
	self recomputeSelection.
	self  announce: (RubTextStyleChanged morph: self).
