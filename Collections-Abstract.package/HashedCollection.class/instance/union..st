enumerating
union: aCollection
	"Answer the set theoretic union of the receiver and aCollection, using the receiver's notion of equality and not side effecting the receiver at all."

	^ self copy addAll: aCollection; yourself