action creators
pullAction
	^ GLMGenericAction new
		action: [ :presentation :repository | 
			repository pull.
			presentation update ];
		icon: GLMUIThemeExtraIcons glamorousLeftSide;
		shortcut: $p;
		title: 'Pull'; 
		showTitle
