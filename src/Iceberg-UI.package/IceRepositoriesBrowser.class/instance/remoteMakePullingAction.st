menu actions - remotes
remoteMakePullingAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model makePullRemote: presentation selection. 
			presentation pane browser update ];
		showTitle: 'Make default pull remote'; 
		enabledCondition: [ :presentation :model | 
			(model isPullRemote: presentation selection) not ];
		yourself
