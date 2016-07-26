accessing
characterToGlyphMap
	"used in 
	primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: kernDelta.
	
	Since 'font xTable' using a first font xtable, we could use the same glyph mapping for it'
	
	This should allow a primitive to not fail, because of characterToGlyphMap == nil
	"
	^ (fontArray at: 1) characterToGlyphMap