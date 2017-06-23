menu actions - remotes
remoteRemoveAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model 
				removeRemote: presentation selection
				then: [ presentation pane browser update ] ];
		icon: #remove asIcon;
		showTitle: 'Remove'; 
		yourself
