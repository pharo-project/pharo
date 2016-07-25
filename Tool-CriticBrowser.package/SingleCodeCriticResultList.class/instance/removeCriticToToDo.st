menu
removeCriticToToDo

	criticsModel selectedItems
		ifNotNil: [:items | items do: [ :item |
			cache removeToDo: item forRule: rule ]].
	
	