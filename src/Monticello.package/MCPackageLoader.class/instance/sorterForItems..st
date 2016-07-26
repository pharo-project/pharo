private
sorterForItems: aCollection
	| sorter |
	sorter := MCDependencySorter items: aCollection.
	sorter addExternalProvisions: self provisions.
	^ sorter