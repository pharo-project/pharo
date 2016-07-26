miscellaneous
yourselfNotUsed
	| detector addSearcher |
	detector := self new.
	detector name: 'Doesn''t use the result of a yourself message'.
	addSearcher := RBParseTreeSearcher new.
	addSearcher addSearch: '`@object yourself'
				-> [:aNode :answer | answer or: [aNode isUsed not]].
	detector methodBlock: 
			[:context :result | 
			(addSearcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector