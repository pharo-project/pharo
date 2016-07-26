menu
transformCritic
	
	criticsModel selectedItems ifNotNil: [ :items |
		items do: [:critic | critic change execute ]  displayingProgress: [:critic|]]