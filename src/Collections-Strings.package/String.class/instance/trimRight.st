copying
trimRight
	"Trim separators from the right side of the receiving string."
	
	^ self trimRight: [ :char | char isSeparator ]