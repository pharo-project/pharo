recursive descent
messagePredicate
	"Match a message predicate specification: a selector (presumably
	understood by a Character) enclosed in :'s ."

	| spec negated |
	spec := self inputUpTo: $: errorMessage: ' no terminating ":"'.
	negated := false.
	spec first = $^ 
		ifTrue: [
			negated := true.
			spec := spec copyFrom: 2 to: spec size].
	^RxsMessagePredicate new 
		initializeSelector: spec asSymbol
		negated: negated