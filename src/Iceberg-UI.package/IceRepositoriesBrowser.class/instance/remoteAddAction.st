menu actions - remotes
remoteAddAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model addRemoteThen: [ presentation pane browser update ] ];
		icon: #add asIcon;
		showTitle: 'Add remote'; 
		yourself
