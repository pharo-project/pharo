rendering
renderOn: renderer
	| canvas glyphRenderer |
	
	canvas := renderer canvas.
	glyphRenderer := canvas setFont: (span getAttribute: TxFontAttribute).
	canvas setPaint: (span getAttribute: TxForeColorAttribute).
	
	glyphs ifNil: [ self getGlyphsUsing: glyphRenderer ].
	glyphRenderer renderGlyphs: glyphs.
	
"	canvas setPaint: Color random.
	canvas drawShape: (0@0 corner: advance x @ 3).
"	renderer advance:advance.