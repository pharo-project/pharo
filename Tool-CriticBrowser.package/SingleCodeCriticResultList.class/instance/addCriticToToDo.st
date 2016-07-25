menu
addCriticToToDo

	criticsModel selectedItems
		ifNotNil: [ :items | items do: [ :item |
			cache
				addToDo: item sourceAnchor entity
				forRule: rule ]].
		
