accessing
collapseAll: aMorph
	| allChildren |
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		each isExpanded
			ifTrue: [self collapseAll: each]].
