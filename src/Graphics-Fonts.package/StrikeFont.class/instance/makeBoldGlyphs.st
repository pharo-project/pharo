emphasis
makeBoldGlyphs
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"
	| g bonkForm |
	g := glyphs deepCopy.
	bonkForm := (Form extent: 1 @ 16) fillBlack offset: -1 @ 0.
	self 
		bonk: g
		with: bonkForm.
	glyphs depth = 1 
		ifTrue: 
			[ g 
				copyBits: g boundingBox
				from: g
				at: 1 @ 0
				clippingBox: g boundingBox
				rule: Form under
				fillColor: nil ]
		ifFalse: 
			[ 0 
				to: g width - 2
				do: 
					[ :x | 
					0 
						to: g height - 1
						do: 
							[ :y | 
							(glyphs colorAt: x @ y) = Color white ifFalse: 
								[ g 
									colorAt: (x + 1) @ y
									put: ((glyphs colorAt: (x + 1) @ y) = Color white 
											ifTrue: [ glyphs colorAt: x @ y ]
											ifFalse: [ Color black ]) ] ] ] ].
	glyphs := g.
	self isSynthetic: true.
	fallbackFont ifNotNil: [ fallbackFont := fallbackFont emphasized: 1 ]