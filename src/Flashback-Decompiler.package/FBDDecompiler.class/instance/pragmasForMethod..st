initialize
pragmasForMethod: aCompiledMethod
	^ aCompiledMethod pragmas collect: [ :each |
		RBPragmaNode 
			selector: each keyword 
			arguments: (each arguments collect: [ :arg | RBLiteralNode value: arg ]) ]