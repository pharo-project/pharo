*Morphic-Base-Converting
asStringMorph
	^ StringMorph
		contents: self string
		font: (self fontAt: 1 withStyle: TextStyle default)
		emphasis: (self emphasisAt: 1)