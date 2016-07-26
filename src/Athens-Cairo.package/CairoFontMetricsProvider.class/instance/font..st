accessing
font: aFont
	font := aFont asFreetypeFont.
	cairoFont :=	CairoScaledFont fromFreetypeFont: font.
	extents := cairoFont extents.