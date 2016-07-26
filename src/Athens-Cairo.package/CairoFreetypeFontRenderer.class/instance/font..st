accessing
font: aFreetypeFont 

	font := CairoScaledFont fromFreetypeFont: aFreetypeFont.
	fontExtents := font extents.
	originalFont := aFreetypeFont.
