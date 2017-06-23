menu actions - repositories list
addLocalRepositoryAction
	^ GLMGenericAction new
		action: [ self addLocalRepository ];
		icon: #packageAdd asIcon;
		shortcut: $a;
		title: 'Add local repository'; showTitle