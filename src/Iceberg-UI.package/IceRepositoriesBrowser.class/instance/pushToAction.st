menu actions - selected repository
pushToAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection pushToThen: [ presentation pane browser update ]];
		selectionCondition: #isOperative;
		"icon: GLMUIThemeExtraIcons glamorousRightSide;"
		showTitle: 'Push to...';
		yourself

