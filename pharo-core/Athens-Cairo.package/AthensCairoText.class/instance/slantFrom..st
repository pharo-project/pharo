private
slantFrom: aSymbol
	^ aSymbol caseOf: {
		[#normal]->[ CAIRO_FONT_SLANT_NORMAL ].
 		[#italic]->[   CAIRO_FONT_SLANT_ITALIC ].
		[#oblique]->[ CAIRO_FONT_SLANT_OBLIQUE ] }