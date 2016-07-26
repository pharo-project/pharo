possible bugs
usesAdd
	| detector addSearcher |
	detector := self new.
	detector name: 'Uses the result of an add: message'.
	addSearcher := RBParseTreeSearcher usesResultOfAdd.
	detector methodBlock: 
			[:context :result | 
			(addSearcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector