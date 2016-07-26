events
expandRoots
	"Expand all the receiver's roots"
	self roots
		do: [:each |
			(each canExpand and: [each isExpanded not])
				ifTrue: [each toggleExpandedState]].
	self adjustSubmorphPositions