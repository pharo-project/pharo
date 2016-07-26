as yet unclassified
applyAttributes
	currentAttributes do: [:attr |
		attr emphasizeScanner: self ]. 
	
	currentFont ifNil: [ self setActualFont: self defaultFont ].
	currentColor ifNil: [ self textColor: self defaultColor ].
	