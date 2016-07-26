double dispatch
syntaxMessagePredicate: messagePredicateNode
	"Double dispatch from the syntax tree. 
	Special link can handle predicates."

	^messagePredicateNode negated
		ifTrue: [RxmPredicate new bePerformNot: messagePredicateNode selector]
		ifFalse: [RxmPredicate new bePerform: messagePredicateNode selector]