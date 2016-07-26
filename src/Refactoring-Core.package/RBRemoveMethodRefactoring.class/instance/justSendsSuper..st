preconditions
justSendsSuper: aSelector 
	| matcher parseTree superclass |
	matcher := RBParseTreeSearcher justSendsSuper.
	parseTree := class parseTreeFor: aSelector.
	(matcher executeTree: parseTree initialAnswer: false) ifFalse: [^false].
	parseTree lastIsReturn ifTrue: [^true].
	superclass := class superclass whichClassIncludesSelector: aSelector.
	superclass isNil ifTrue: [^true].	"Since there isn't a superclass that implements the message, we can 
								 delete it since it would be an error anyway."
	parseTree := superclass parseTreeFor: aSelector.
	matcher := RBParseTreeSearcher new.
	matcher matches: '^``@object'
		do: 
			[:aNode :answer | 
			answer
				add: aNode value;
				yourself].
	matcher executeTree: parseTree initialAnswer: Set new.
	^(matcher answer 
		detect: [:each | (each isVariable and: [each name = 'self']) not]
		ifNone: [nil]) isNil