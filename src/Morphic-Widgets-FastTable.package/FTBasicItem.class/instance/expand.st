expanding-collapsing
expand
	"I do not refresh the table. Use expandAndRefresh for that."

	isExpanded := true.
	recentlyChanged := true.
	self dataSource
		updateSelectionWithCollectBlock: [ :indexOfSelection :indexOfSelf | 
			indexOfSelection <= indexOfSelf
				ifTrue: [ indexOfSelection ]
				ifFalse: [ indexOfSelection + self numberOfVisibleChildren ] ]