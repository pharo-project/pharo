merging
mergeMap

	| map |
	map := super mergeMap.
	map at: #requires put: requires.
	map at: #includes put: includes.
	map at: #answers put: answers.
	^map