accessing
sortedVariables
	"sort variables for comparison purposes"

	| sorted |
	sorted := variables select: [:var | var isOrderDependend].
	sorted addAll: ((variables reject: [:var | var isOrderDependend])
		asSortedCollection: [:a :b | a name <= b name]).
	^sorted