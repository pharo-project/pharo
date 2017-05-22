menu actions - package
unloadPackageAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection unload.
			presentation update ]; 
		icon: #delete asIcon;
		shortcut: $u; 
		title: 'Unload package'; 
		condition: [ :presentation | 
			presentation selection notNil and: [ presentation selection isLoaded ]];
		yourself