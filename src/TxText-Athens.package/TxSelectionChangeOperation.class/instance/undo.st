do/undo
undo
	editor selection start moveRight: startDelta negated.
	editor selection end moveRight: endDelta negated.
	editor notifySelectionChange: editor selection