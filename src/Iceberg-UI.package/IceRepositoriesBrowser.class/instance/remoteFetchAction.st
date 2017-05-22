menu actions - remotes
remoteFetchAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model fetchRemote: presentation selection. 
			presentation pane browser update ];
		icon: #glamorousLeftSide asIcon;
		showTitle: 'Fetch'; 
		yourself