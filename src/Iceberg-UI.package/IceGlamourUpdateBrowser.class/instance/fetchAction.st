action creators
fetchAction
	^ GLMGenericAction new
		action: [ :presentation :repository | 
			repository fetch.
			presentation update ];
		icon: GLMUIThemeExtraIcons glamorousLeftSide;
		shortcut: $f;
		title: 'Fetch new versions'; 
		showTitle
