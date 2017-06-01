instance creation
fromFreetypeFont: aFont
	| ftFace face |
	^ CairoBackendCache soleInstance at: aFont ifAbsentPut: [ | emphasis |
			emphasis := aFont simulatedEmphasis.
			ftFace := aFont face.
			face := CairoBackendCache soleInstance 
				at: {ftFace.	emphasis} 
				ifAbsentPut: [ | cff |
					cff := CairoFontFace fromFreetypeFace: ftFace.
					emphasis ifNotNil: [ cff synthesizeEmphasis: emphasis ].
					cff ].
			self fromFreetypeFont: aFont cairoFace: face ]