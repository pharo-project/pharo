*AST-Core
parseTree
	^(RBParser 
		parseMethod: self sourceCode 
		onError: [ :msg :pos | 
			^ self decompile ]) methodClass: self methodClass.