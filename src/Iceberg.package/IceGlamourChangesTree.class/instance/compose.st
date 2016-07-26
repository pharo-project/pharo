building
compose
	self fastTree 
		title: 'Local changes';
		allExpanded;
		children: #elements;
		icon: #icon;
		format: #description;
		display: #elements;
		updateOn: IceChangeSetChanged from: #announcer;
		addAction: (GLMGenericAction new
			action: [ :presentation :entity | entity refresh ];
			icon: GLMUIThemeExtraIcons glamorousRefresh;
			shortcut: $r;
			title: 'Refresh'; showTitle)
