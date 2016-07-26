styling
styleCode: aCode inContext: aContext isMethod: isMethod
	| ast |
	
	ast := isMethod 
		ifTrue: [
			[ RBParser parseFaultyMethod: aCode asString ] 
			on: Error 
			do: [ RBParser parseFaultyExpression: aCode asString] ]
		ifFalse: [ RBParser parseFaultyExpression: aCode asString ].
	
	aContext ifNotNil: [  
		ast methodClass: aContext ].
	ast doSemanticAnalysis.
	text := ast source asTxModel.
	
	ast acceptVisitor: self.
	
	^ text
