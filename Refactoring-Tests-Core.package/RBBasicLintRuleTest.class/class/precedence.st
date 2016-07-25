miscellaneous
precedence
	| detector matcher |
	detector := self new.
	detector name: 'Inspect instances of "A + B * C" might be "A + (B * C)"'.
	matcher := RBParseTreeSearcher new.
	matcher addSearches: #('``@A + ``@B * ``@C' '``@A - ``@B * ``@C')
				-> [:aNode :answer | answer or: [aNode receiver parentheses isEmpty]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector