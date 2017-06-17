action creations
inspectAction
	^ GLMGenericAction new
		action: [ :presentation | presentation selection inspect ]; 
		shortcut: $i; 
		title: 'Inspect'; 
		yourself

