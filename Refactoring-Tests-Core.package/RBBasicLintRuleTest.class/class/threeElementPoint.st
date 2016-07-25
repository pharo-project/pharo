possible bugs
threeElementPoint
	| detector matcher |
	detector := self new.
	detector name: 'Possible three element point (e.g., x @ y + q @ r)'.
	matcher := (RBParseTreeSearcher new) addSearch: '``@x @ ``@y' -> 
								[:aNode :answer | 
								answer or: 
										[| current |
										current := aNode parent.
										[current isNil or: 
												[current isMessage
													and: [current selector == #@ or: [current selector isInfix not]]]]
											whileFalse: [current := current parent].
										(current isNil or: [current isMessage and: [current selector isInfix not]])
											not]];
				yourself.
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector