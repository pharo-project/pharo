action
unexpandAllChildOf: anItem
	anItem children
		do: [ :item | 
			item isExpanded
				ifTrue: [ self unexpandAllChildOf: item ].
			item collapse ]  displayingProgress: [:each | 'Rearanging ' , each name]