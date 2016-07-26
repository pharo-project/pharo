merging
mergeMap

	| map |
	map := super mergeMap.
	map at: #value put: value.
	^map