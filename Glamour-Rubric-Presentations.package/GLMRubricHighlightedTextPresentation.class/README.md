|browser|
browser := GLMTabulator new.
browser row: #row.

browser transmit to: #row; andShow: [ :composite |
	composite custom: (GLMRubricHighlightedTextPresentation new
		editingMode: [GLMHighlighterTextRubEditingMode 
			withStylerDecorator: (RbEPersonDecorator new
				styler:  (GLMHighlighterTextParserStyler new
					parser: RbEPersonColorizer new))]) ].
	
browser openOn: '
person {	
	name	=	''Phil'';
	age = ''23'';	
}
person {}
'.