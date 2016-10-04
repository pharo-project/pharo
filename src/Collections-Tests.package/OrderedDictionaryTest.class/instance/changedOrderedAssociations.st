accessing
changedOrderedAssociations
	^ self orderedAssociations collect: [:each | each key -> self newValue]