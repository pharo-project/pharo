double dispatch
syntaxCharacter: charNode
	"Double dispatch from the syntax tree. 
	We get here when no merging characters into strings was possible."

	| wanted |
	wanted := charNode character.
	^RxmPredicate new predicate: 
		(ignoreCase
			ifTrue: [[:char | char sameAs: wanted]]
			ifFalse: [[:char | char = wanted]])