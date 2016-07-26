possible bugs
missingYourself
	| detector matcher |
	detector := self new.
	detector name: 'Possible missing "; yourself"'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearch: '``@xobject `@messages: ``@args' -> 
					[:aNode :answer | 
					answer or: 
							[aNode parent isCascade
								and: [aNode isDirectlyUsed and: [aNode selector ~~ #yourself]]]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector