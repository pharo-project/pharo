displaying
displayStrikeoutOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint
	| top bottom strikeoutThickness s e |
	
	"the strikeout size/position for TrueType fonts should really come from the TT_OS2 table.
	This needs to be read by the plugin when the face is created.
	For now, we use the underlineThickness, and 1/4 of the ascender from the baseline"
	strikeoutThickness := (self face underlineThickness * self pixelSize / self face unitsPerEm). 			
	top := ((self face ascender / 4) * self pixelSize / self face unitsPerEm) negated - (strikeoutThickness/2).
	top := top rounded.
	bottom := top + strikeoutThickness ceiling.
	s := baselineStartPoint + (0@top).
	e := baselineEndPoint + (0@bottom).
	self displayLineGlyphOn: aDisplayContext from: s to: e
	