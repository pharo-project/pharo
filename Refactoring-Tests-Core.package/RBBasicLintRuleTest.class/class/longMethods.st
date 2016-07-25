miscellaneous
longMethods
	| detector matcher |
	detector := self new.
	detector name: 'Long methods'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearch: '`.Stmt' -> 
					[:aNode :answer | 
					(aNode children inject: answer
						into: [:sum :each | matcher executeTree: each initialAnswer: sum]) + 1].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: 0)
				>= self longMethodSize
					ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector