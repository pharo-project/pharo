comparing
<=  aTextStyleAsFontFamilyMember
	| orderedItems |
	orderedItems := #('Condensed' 'Condensed Italic' 'Condensed Bold'  'Condensed Bold Italic' 'Regular' 'Italic' 'Bold' 'Bold Italic').
	^(orderedItems indexOf: self styleName) <= (orderedItems indexOf: aTextStyleAsFontFamilyMember styleName)