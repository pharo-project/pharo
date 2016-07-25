events
expandAll: aMorph
	| allChildren |
	aMorph toggleExpandedState.
	allChildren := OrderedCollection new: 10.
	aMorph recursiveAddTo: allChildren.
	allChildren do: [:each | 
		(each canExpand and: [each isExpanded not])
			ifTrue: [self expandAll: each]].
