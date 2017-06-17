menu actions - selected repository
switchBranchAction
	^ GLMGenericAction new
		action: [ :presentation |
			presentation selection ifNotNil: [:repo | repo switchBranch ].
			presentation pane browser pane update ];
		enabledCondition: [ :presentation | 
			presentation selection notNil ];	
		selectionCondition: #isOperative;
		showTitle: 'Switch branch...';
		shortcut: $b.