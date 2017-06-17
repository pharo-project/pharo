menu actions - package
reloadAllPackagesAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model reloadAllLoadedPackages. 
			presentation update ];
		condition: [ :presentation :repo | (repo entity savedPackages select: #isLoaded) notEmpty ];
		icon: #refresh asIcon;
		shortcut: $e;
		title: 'Reload all'; showTitle