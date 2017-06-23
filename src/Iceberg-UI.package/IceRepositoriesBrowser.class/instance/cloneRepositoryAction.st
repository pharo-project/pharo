menu actions - repositories list
cloneRepositoryAction
	^ GLMGenericAction new
		action: [ self cloneRepository ];
		icon: #add asIcon;
		shortcut: $c;
		title: 'Clone repository'; showTitle