action creators
synchronizeRepositoryAction
	^ GLMGenericAction new
		action: [ :table | table selection 
			ifNotNil: [:selection | IceGlamourSynchronizer synchronize: selection ]
			ifNil: [ self abort: 'Please select a repository to synchronize.' ]];
		icon: GLMUIThemeExtraIcons glamorousBrowse;
		shortcut: $s;
		title: 'Synchronize with repository'; showTitle