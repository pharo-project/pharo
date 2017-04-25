expanding-collapsing
collapse
	"I do not refresh the table. Use collapseAndRefresh for that. I also do not 
	ensure that any selected element is visible after collapsing the element.
	If any selected elements are within the childrens of the collapsed element,
	I select the collapsed element."

	recentlyChanged := true.
	self dataSource
		updateSelectionWithCollectBlock:
			[ :indexOfCurrentSelection :indexOfCollapsedElement | 
			| changedBy |
			changedBy := self numberOfVisibleChildren.
			(indexOfCurrentSelection between: indexOfCollapsedElement and: indexOfCollapsedElement + changedBy)
				ifTrue: [ indexOfCollapsedElement ]
				ifFalse: [ indexOfCurrentSelection < indexOfCollapsedElement
						ifTrue: [ indexOfCurrentSelection ]
						ifFalse: [ indexOfCurrentSelection - changedBy ] ] ].
	isExpanded := false