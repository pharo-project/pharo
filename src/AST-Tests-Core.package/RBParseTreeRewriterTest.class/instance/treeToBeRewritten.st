setup
treeToBeRewritten
	^ RBParser
		parseMethod:
			'method: asdf
	<primitive: 1>
	<primitive: 2>
	^asdf + self foo + asdf'