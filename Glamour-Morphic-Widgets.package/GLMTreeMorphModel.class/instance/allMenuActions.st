accessing
allMenuActions
	^ (self glamourPresentation allSelectionActions select: [:action | action hasTitle ]) 