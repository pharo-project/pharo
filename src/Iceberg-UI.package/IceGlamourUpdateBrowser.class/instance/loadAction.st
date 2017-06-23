action creators
loadAction
	^ GLMGenericAction new
		action: [ :presentation | presentation selection load. presentation selection: nil ];
		condition: [ :presentation | presentation selection notNil and: [ presentation selection isMerged ]];
		icon: (Smalltalk ui icons iconNamed: #changeBlock);
		shortcut: $l;
		title: 'Load'; showTitle