actions
browseReferencesAction
	^ GLMGenericAction new
		action: [ :presentation | presentation selection browseReferences ];
		condition: [ :presentation | 
			presentation selection notNil 
				and: [ presentation selection canBrowseReferences ]];
		shortcut: $n;
		title: 'Browse references'; 
		showTitle