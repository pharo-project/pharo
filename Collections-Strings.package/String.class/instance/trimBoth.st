copying
trimBoth
	"Trim separators from both sides of the receiving string."
	
	^ self trimBoth: [ :char | char isSeparator ]