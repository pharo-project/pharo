accessing
allActionsWithShortcuts 
	^ (self allActions, self allSelectionActions) select: [ :action | action hasShortcut ]