visiting
visitBraceNode: aBraceNode
	aBraceNode elements do:
		[:element| element accept: self]