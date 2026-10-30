Extension { #name : 'OupsDebuggerSelectionStrategy' }

{ #category : '*System-Settings-Core' }
OupsDebuggerSelectionStrategy class >> debuggerSelectionStrategySettingsOn: aBuilder [

	<systemsettings>
	(aBuilder pickOne: #debuggerSelectionStrategy)
		label: 'Debugger selection strategy';
		target: self;
		defaultValue: self defaultDebuggerSelectionStrategy;
		parentName: #debugging;
		domainValues: self allSubclasses;
		description: 'Sets the strategy to open a debugger.'
]
