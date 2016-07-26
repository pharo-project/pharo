double dispatch
syntaxAny
	"Double dispatch from the syntax tree. 
	Create a matcher for any non-null character."

	^RxmPredicate new
		predicate: [:char | char asInteger ~= 0]