initialization
initializePresenter
	super initializePresenter.
	
	activeLogLabelModel
		label: '  Writing into:'.
		
	activeLogTextModel
		disable.
		
	createActiveLogButtonModel
		icon: (self iconNamed: #addIcon);
		help: 'Start recording into new Log';
		action: [ self createNewActiveLog ].

	logSetButtonModel 
		icon: EpLogSetModel icon;
		label: 'Log Set';
		action: [ self openLogSet ].
		
	logModel
		viewClass: EpLogBrowser defaultViewClass.
		
	enableMonitorButtonModel
		label: '-';
		action: [ 
			enableMonitorButtonModel state: enableMonitorButtonModel state not.
			self monitor enabled: enableMonitorButtonModel state.
			self refresh ].
	
	self whenWindowChanged: [ :windowModel |
		windowModel whenClosedDo: [ logModel unsubscribeFromLog ] ].