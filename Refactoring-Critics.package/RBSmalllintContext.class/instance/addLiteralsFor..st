private
addLiteralsFor: aCompiledMethod
	2 to: aCompiledMethod numLiterals - 1
		do: [ :index | self checkLiteral: (aCompiledMethod objectAt: index) ]