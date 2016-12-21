initialization
initializeValueHolders
	displaySymbol := #displayString asValueHolder.
	entity := nil asValueHolder.
	ghostText := 'Choose one' asValueHolder.
	possibleEntities := #() asValueHolder.
	selectDialog := [  ] asValueHolder