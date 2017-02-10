expanding-collapsing
expand
	"I do not refresh the table. Use expandAndRefresh for that. I also do not 
	ensure that any selected element is visible after expanding the element."

	isExpanded := true.
	recentlyChanged := true.
	self dataSource
		updateSelectionWithCollectBlock: [ :indexOfCurrentSelection :indexOfCollapsedElement |
			indexOfCurrentSelection <= indexOfCollapsedElement 
				ifTrue: [ indexOfCurrentSelection ]
				ifFalse: [ indexOfCurrentSelection + self numberOfVisibleChildren ] ]