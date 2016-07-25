public access
parse
	| expression selector arguments method |
	self compilationContext noPattern ifFalse: [ ^ self parseMethod ].
	expression := self parseExpression asSequenceNode transformLastToReturn.
	context
		ifNil: [ 
			selector := #DoIt.
			arguments := #() ] 
		ifNotNil: [ 
			selector := #DoItIn:.
			arguments := { RBVariableNode named: 'ThisContext' } ].
	method := RBMethodNode selector: selector arguments: arguments body: expression.
	context ifNotNil: [ method rewriteTempsForContext: context ].
	^ method