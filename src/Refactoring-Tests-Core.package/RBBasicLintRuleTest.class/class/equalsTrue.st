unnecessary code
equalsTrue
	| detector matcher |
	detector := self new.
	detector name: 'Unnecessary "= true"'.
	matcher := (RBParseTreeSearcher new) addSearches: #('true' 'false') -> 
								[:aNode :answer | 
								answer or: 
										[aNode parent isMessage
											and: [#(#= #== #~= #~~) includes: aNode parent selector]]];
				yourself.
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector