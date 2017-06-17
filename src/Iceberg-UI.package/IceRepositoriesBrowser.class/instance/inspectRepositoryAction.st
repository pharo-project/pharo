menu actions - selected repository
inspectRepositoryAction
	^ GLMGenericAction new
		selectionAction: #inspectRepository;
		showTitle: 'Inspect';
		shortcut: $i;
		yourself
