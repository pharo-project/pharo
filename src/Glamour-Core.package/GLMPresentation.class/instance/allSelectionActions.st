accessing
allSelectionActions 
	^ (self selectionActions, self dynamicActionsOnSelection)
			select: [ :each | each isAvailableOn: self ]