action creators
pushAction
	 ^ GLMGenericAction new
			action: [ :presentation :repository | 
				self 
					pushRepository: repository
					andThen: [ presentation selection: nil ]];
			icon: GLMUIThemeExtraIcons glamorousRightSide;
			shortcut: $p;
			showTitle: 'Publish all'
				
