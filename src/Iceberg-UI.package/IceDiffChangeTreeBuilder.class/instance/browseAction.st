actions
browseAction
	^ GLMGenericAction new
		action: [ :presentation | presentation selection browse ];
		condition: [ :presentation :entity | 
			presentation selection notNil 
				and: [ presentation selection canBeBrowsed ]];
		icon: #nautilus asIcon;
		shortcut: $b;
		title: 'Browse'; 
		showTitle