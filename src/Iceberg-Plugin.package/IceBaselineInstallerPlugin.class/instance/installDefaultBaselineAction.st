private actions
installDefaultBaselineAction
	^ GLMGenericAction new
		action: [ :presentation | self installDefaultBaseline: presentation selection entity ];
		showTitle: 'Install baseline (default)'; 
		yourself	