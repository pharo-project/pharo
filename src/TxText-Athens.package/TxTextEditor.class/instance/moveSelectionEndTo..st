navigation
moveSelectionEndTo: aTextPosition
	(aTextPosition hasSamePositionAs: selection end)
		ifFalse: [ self recordSelectionChange: [ selection end: aTextPosition ] ].
