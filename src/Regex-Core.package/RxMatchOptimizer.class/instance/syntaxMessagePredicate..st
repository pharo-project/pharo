double dispatch
syntaxMessagePredicate: messagePredicateNode 
	messagePredicateNode negated
		ifTrue: [nonMethodPredicates add: messagePredicateNode selector]
		ifFalse: [methodPredicates add: messagePredicateNode selector]