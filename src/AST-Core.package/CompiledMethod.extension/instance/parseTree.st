*AST-Core
parseTree
	^(RBParser 
		parseMethod: self sourceCode 
		onError: [ :msg :pos | 
			^ RBParser 
				parseMethod: self decompile ]) methodClass: self methodClass.