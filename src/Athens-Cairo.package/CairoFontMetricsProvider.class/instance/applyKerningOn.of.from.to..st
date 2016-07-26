private
applyKerningOn: aGlyphs of: aString from: start to: end
	|kerning  previous|
	kerning := 0.
	previous := nil.
	aGlyphs doWithIndex: [ :glyph :index | |current|
		current := aString at: start + (index - 1).
		index > 1 ifTrue: [
			kerning := kerning + (font kerningLeft: previous right: current).
			glyph x: glyph x + kerning ].
		previous := current ]