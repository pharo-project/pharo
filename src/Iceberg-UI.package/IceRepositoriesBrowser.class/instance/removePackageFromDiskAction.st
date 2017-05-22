menu actions - package
removePackageFromDiskAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection 
				removeFromDiskThen: [ presentation update ] ]; 
		icon: #smallDelete asIcon;
		title: 'Remove package from disk'; 
		condition: [ :presentation | 
			presentation selection notNil and: [ presentation selection isLoaded not ] ];
		yourself

