action creators
loadPackageAction
	^ GLMGenericAction new
		action: [ :presentation :repo | 
			presentation selection loadLatest.
			presentation update
		]; 
		icon: (Smalltalk ui icons iconNamed: #add);
		shortcut: $l; 
		title: 'Load package'; 
		condition: [ :presentation | presentation selection notNil and: [ presentation selection isLoaded not ]];
		yourself

