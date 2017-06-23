menu actions - remotes
remoteMakePushingAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model makePushRemote: presentation selection. 
			presentation pane browser update ];
		showTitle: 'Make default push remote'; 
		enabledCondition: [ :presentation :model | 
			(model isPushRemote: presentation selection) not ];
		yourself
