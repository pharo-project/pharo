accessing text
textFont: aFont
	self addAttribute: (TextFontReference toFont: aFont).
	self paragraph compose.
	self recomputeSelection.
	self  announce: (RubTextStyleChanged morph: self).
