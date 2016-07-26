private
applyKerningOn: aGlyphs of: aString from: start to: end
	"Apply kerning on a glyphs that represent aString from
	start to end"
	|kerning  previous|
	kerning := 0.
	previous := nil.
	aGlyphs doWithIndex: [ :glyph :index | |current|
		current := aString at: start + (index - 1).
		index > 1 ifTrue: [
			kerning := kerning + (originalFont kerningLeft: previous right: current).
			glyph x: glyph x + kerning ].
		previous := current ]