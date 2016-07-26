merging
merge: aCollection With: anOtherCollection
	^ aCollection asSet
		addAll: anOtherCollection;
		yourself