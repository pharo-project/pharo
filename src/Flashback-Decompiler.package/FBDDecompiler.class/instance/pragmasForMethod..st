initialize
pragmasForMethod: arg1
	^ arg1 pragmas
		collect: [ :arg2 | 
			RBPragmaNode
				selector: arg2 selector
				arguments: (arg2 arguments collect: [ :arg3 | RBLiteralNode value: arg3 ]) ]