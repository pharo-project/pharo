initialization
initialize
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher matches: '`.Stmt'
		do: 
			[:aNode :answer | 
			(aNode children inject: answer
				into: [:sum :each | matcher executeTree: each initialAnswer: sum]) + 1].