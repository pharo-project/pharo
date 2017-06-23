menu actions - selected repository
forgetRepositoryAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection forgetThen: [ 
				presentation pane browser pane update ] ];
		showTitle: 'Forget repository...'; 
		shortcut: $x; 
		yourself
