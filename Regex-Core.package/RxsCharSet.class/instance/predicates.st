accessing
predicates

	^(elements reject: [:some | some isEnumerable])
		collect: [:each | each predicate]