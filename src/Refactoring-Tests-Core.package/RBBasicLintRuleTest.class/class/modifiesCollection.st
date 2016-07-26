possible bugs
modifiesCollection
	| detector addSearcher |
	detector := self new.
	detector name: 'Modifies collection while iterating over it'.
	addSearcher := RBBasicLintRuleTest modifiesCollection.
	detector methodBlock: 
			[:context :result | 
			addSearcher executeTree: context parseTree initialAnswer: false.
			addSearcher answer 
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector