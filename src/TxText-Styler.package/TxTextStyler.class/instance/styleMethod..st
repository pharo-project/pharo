styling
styleMethod: aMethod
	| ast |
	
	ast := aMethod ast.
	text := ast source asTxModel.
	
	ast acceptVisitor: self.
	
	^ text
