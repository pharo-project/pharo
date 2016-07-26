enumerating
union: aCollection
	"Answer the set theoretic union of two collections."
	
	| set |
	
	set := self asSet addAll: aCollection; yourself.
	
	^ self species withAll: set asArray