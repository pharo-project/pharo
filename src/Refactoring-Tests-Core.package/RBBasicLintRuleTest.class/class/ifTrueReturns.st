intention revealing
ifTrueReturns
	| detector matcher |
	detector := self new.
	detector name: 'ifTrue:/ifFalse: returns instead of and:/or:''s'.
	matcher := RBParseTreeSearcher new.
	matcher addSearches:
		#('| `@temps | ``@.Statements. ``@object ifTrue: [^``@value1]. ^``@value2' 
		'| `@temps | ``@.Statements. ``@object ifFalse: [^``@value1]. ^``@value2') 
			-> [:aNode :answer | answer
				or: 
					[| node |
					node := (aNode statements at: aNode statements size - 1) arguments first body statements last value. "``@value1"
					(node isLiteralNode and: [{true. false} includes: node value])
						or: 
							[node := aNode statements last value.
							node isLiteralNode and: [{true. false} includes: node value]]]].
	detector methodBlock: [:context :result | (matcher executeTree: context parseTree initialAnswer: false)
			ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector