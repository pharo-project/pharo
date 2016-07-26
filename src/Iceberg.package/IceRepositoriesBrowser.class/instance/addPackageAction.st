action creators
addPackageAction
	^ GLMGenericAction new
		action: [ :presentation :repo | self class addPackageToRepository: repo ];
		icon: GLMUIThemeExtraIcons glamorousAdd;
		shortcut: $a;
		title: 'Add package'; showTitle