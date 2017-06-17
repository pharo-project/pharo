menu actions - package
addPackageAction
	^ GLMGenericAction new
		action: [ :presentation :repo | 
			self addPackageToRepository: repo repository.
			presentation update ];
		icon: #add asIcon;
		shortcut: $a;
		title: 'Add package'; showTitle