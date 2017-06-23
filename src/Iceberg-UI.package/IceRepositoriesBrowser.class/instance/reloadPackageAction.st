menu actions - package
reloadPackageAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection reload.
			presentation update ]; 
		icon: #refresh asIcon;
		shortcut: $r; 
		title: 'Reload package'; 
		condition: [ :presentation | presentation selection notNil and: [ presentation selection isLoaded ]];
		yourself

