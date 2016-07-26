miscellaneous
ifTrueBlocks
	| detector matcher |
	detector := self new.
	detector name: 'Non-blocks in ifTrue:/ifFalse: messages'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearches: #('``@condition ifTrue: ``@block' '``@condition ifFalse: ``@block' '``@condition ifTrue: ``@block1 ifFalse: ``@block2' '``@condition ifFalse: ``@block1 ifTrue: ``@block2')
				-> 
					[:aNode :answer | 
					answer or: 
							[(aNode arguments detect: [:each | each isBlock not] ifNone: [nil]) notNil]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector