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
		help: 'Browse all changes (not only this changes of this session but potentionally from other sessions)';
		icon: EpLogSetModel icon;
		label: EpLogSetModel shortTitle;
		action: [ self openLogSet ].
		
	logModel
		viewClass: EpLogBrowser defaultViewClass.
		
	enableMonitorButtonModel
		help: 'Enable/disable the Epicea Monitor. When enabled, it logs code changes (and some IDE events such as MC load/save and refactorings) performed in this Pharo session.';
		label: '-';
		action: [ 
			enableMonitorButtonModel state: enableMonitorButtonModel state not.
			self monitor enabled: enableMonitorButtonModel state.
			self refresh ].
	
	self whenWindowChanged: [ :windowModel |
		windowModel whenClosedDo: [ logModel unsubscribeFromLog ] ].