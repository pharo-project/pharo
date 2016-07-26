private
allKeyboardActionsFor: aPresentation 
	^(aPresentation allActions, 
		aPresentation allSelectionActions) 
		select: [ :action | action hasShortcut ]