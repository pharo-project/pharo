protected
materializeLiteralsTo: compiledMethod with: aDecoder

	1 to: compiledMethod numLiterals 
		do: [ :index | compiledMethod literalAt: index put: aDecoder nextEncodedReference ]
