running
basicCheck: anAST
	self 
		deprecated: 'Please subclass from ReNodeBasedRule and treat the passed entity as in can be any AST node'
		on: 	'11 July 2016' 
		in: 'Pharo-6.0-60143-'.
	^ super basicCheck: anAST 