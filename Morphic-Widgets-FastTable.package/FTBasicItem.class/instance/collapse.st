expanding-collapsing
collapse
	"I do not refresh the table. Use collapseAndRefresh for that."

	recentlyChanged := true.
	self dataSource
		updateSelectionWithCollectBlock:
			[ :ind :index | 
			| changedBy |
			changedBy := self numberOfVisibleChildren.
			(ind between: index and: index + changedBy)
				ifTrue: [ index ]
				ifFalse: [ ind < index
						ifTrue: [ ind ]
						ifFalse: [ ind - changedBy ] ] ].
	isExpanded := false