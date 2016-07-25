styling
restyle: aText inContext: aContext isMethod: isMethod
	| ast code |
	
	code := aText asString.
	ast := isMethod 
		ifTrue: [
			[ RBParser parseFaultyMethod: code ] 
			on: Error 
			do: [ RBParser parseFaultyExpression: code ] ]
		ifFalse: [ RBParser parseFaultyExpression: code ].
	
	aContext ifNotNil: [  
		ast methodClass: aContext ].
	ast doSemanticAnalysis.
	text := aText.
	
	ast acceptVisitor: self.
	
	^ text
