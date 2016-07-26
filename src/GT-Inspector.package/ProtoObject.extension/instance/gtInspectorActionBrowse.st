*GT-Inspector
gtInspectorActionBrowse
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ Smalltalk tools browser openOnClass: self class ];
		icon: GLMUIThemeExtraIcons glamorousBrowse;
		title: 'Browse'