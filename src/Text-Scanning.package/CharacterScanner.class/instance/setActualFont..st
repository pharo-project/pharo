text attributes
setActualFont: aFont
	"Set the basal font to an isolated font reference."

	xTable := aFont xTable.
	map := aFont characterToGlyphMap.
	font := aFont.