merging
mergeMap

	| map |
	map := super mergeMap.
	map at: #projectReference put: projectReference.
	^map