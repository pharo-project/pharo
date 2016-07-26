text attributes
addEmphasis: emphasis
	| fnt |
	
	fnt := currentFont ifNil: [ self defaultFont ] ifNotNil: [ currentFont font ].
	
	self setActualFont: (fnt emphasized: emphasis)