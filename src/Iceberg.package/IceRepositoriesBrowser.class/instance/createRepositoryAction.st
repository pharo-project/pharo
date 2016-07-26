action creators
createRepositoryAction
	^ GLMGenericAction new
		action: [ :presentation | self class createRepository ifNotNil: [ presentation pane ]];
		icon: GLMUIThemeExtraIcons glamorousAdd;
		shortcut: $a;
		title: 'Add repository'; showTitle