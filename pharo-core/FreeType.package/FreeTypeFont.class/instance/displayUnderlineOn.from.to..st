displaying
displayUnderlineOn: aDisplayContext from: baselineStartPoint to: baselineEndPoint
	| underlineTop underlineBottom underlineThickness s e |

	underlineThickness := (self face underlineThickness * self pixelSize / self face unitsPerEm). 
	underlineTop := (self face underlinePosition * self pixelSize / self face unitsPerEm) negated - (underlineThickness/2).
	underlineTop := underlineTop rounded + 1.  "needs the +1 , possibly because glyph origins are moved down by 1 so that their baselines line up with strike fonts"
	underlineBottom := underlineTop + underlineThickness ceiling.
	s := baselineStartPoint + (0@underlineTop).
	e := baselineEndPoint + (0@(underlineBottom)).
	self displayLineGlyphOn: aDisplayContext from: s to: e