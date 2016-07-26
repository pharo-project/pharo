comparing
= aBorderStyle
	^self species = aBorderStyle species
		and:[self style == aBorderStyle style
		and:[self width = aBorderStyle width
		and:[self color = aBorderStyle color]]].