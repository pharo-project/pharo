model access
selectionInterval: sel
	selectionInterval := sel.
	setSelectionSelector ifNotNil: [ model perform:setSelectionSelector with: sel ].