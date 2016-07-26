double dispatch
syntaxPredicate: predicateNode
	"Double dispatch from the syntax tree. 
	A character set is a few characters, and we either match any of them,
	or match any that is not one of them."

	^RxmPredicate with: predicateNode predicate