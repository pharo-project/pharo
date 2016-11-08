private-actions
selectNext
	| index nextSelection |
	
	index := tabs indexOf: self selectedTab.
	nextSelection := tabs at: index + 1 ifAbsent: [ ^ self ].

	nextSelection selected: true.