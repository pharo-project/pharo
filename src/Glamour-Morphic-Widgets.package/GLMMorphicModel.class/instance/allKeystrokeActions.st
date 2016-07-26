callbacks
allKeystrokeActions
	^ (self glamourPresentation allActions, self glamourPresentation allSelectionActions) select: [ :action | 
		action hasShortcut ]