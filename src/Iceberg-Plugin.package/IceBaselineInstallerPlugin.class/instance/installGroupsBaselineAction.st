private actions
installGroupsBaselineAction
	^ GLMGenericAction new
		action: [ :presentation | self installBaselineGroups: presentation selection entity ];
		showTitle: 'Install baseline...'; 
		yourself	