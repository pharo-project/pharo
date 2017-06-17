menu actions - repositories list
fetchAllRepositoriesAction
	^ GLMGenericAction new 
		action: [ self fetchAllRepositories ];
		icon: #glamorousLeftSide asIcon;
		shortcut: $F;
		showTitle: 'Fetch all'
		