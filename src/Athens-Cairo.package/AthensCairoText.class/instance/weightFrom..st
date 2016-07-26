private
weightFrom: aSymbol
	^ aSymbol caseOf: {
		[#normal]->[ CAIRO_FONT_WEIGHT_NORMAL  ].
 		[#bold]->[    CAIRO_FONT_WEIGHT_BOLD ] }
