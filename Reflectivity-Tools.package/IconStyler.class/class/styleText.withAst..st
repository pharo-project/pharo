api
styleText: textModel withAst: ast
	self allSubclassesDo: [ :styleClass | 
			styleClass shouldStyle
				ifTrue: [ ast acceptVisitor: (styleClass style: textModel) ] ]