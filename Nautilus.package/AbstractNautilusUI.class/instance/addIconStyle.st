styling
addIconStyle

	self selectedMethod ifNil: [ ^self ].
	IconStyler styleText: self sourceTextModel withAst: self selectedMethod ast.