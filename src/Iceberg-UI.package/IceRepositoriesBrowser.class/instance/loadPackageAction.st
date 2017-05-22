menu actions - package
loadPackageAction
	^ GLMGenericAction new
		action: [ :presentation :repo | 
			presentation selection loadLatest.
			presentation update ]; 
		icon: #add asIcon;
		shortcut: $l; 
		title: 'Load package'; 
		condition: [ :presentation | presentation selection notNil and: [ presentation selection isLoaded not ]];
		yourself

