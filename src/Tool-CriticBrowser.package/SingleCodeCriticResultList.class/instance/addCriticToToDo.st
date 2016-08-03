menu
addCriticToToDo

	criticsModel selectedItems
		ifNotNil: [ :items | items do: [ :item |
			cache
				addToDo: item
				forRule: rule ]].
		
