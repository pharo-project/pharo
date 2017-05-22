menu actions - package
removePackageAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection 
				removeFromDiskThen: [ presentation update ] ]; 
		icon: #smallDelete asIcon;
		title: 'Remove package from repository (do not unload)'; 
		condition: [ :presentation | 
			presentation selection notNil and: [ presentation selection isLoaded ] ];
		yourself

