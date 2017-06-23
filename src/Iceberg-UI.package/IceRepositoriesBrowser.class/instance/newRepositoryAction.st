menu actions - repositories list
newRepositoryAction
	^ GLMGenericAction new
		action: [ :presentation | self newRepository ];
		icon: #smallNew asIcon;
		shortcut: $n;
		title: 'New repository'; showTitle