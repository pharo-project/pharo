menu
removeCriticToFalsePositive
	
	criticsModel selectedItems
		ifNotNil: [ :items | items do: [ :item |
			cache removeFalsePositive: item forRule: rule ]].
	