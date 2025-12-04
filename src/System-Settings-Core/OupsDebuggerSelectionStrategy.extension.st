Extension { #name : 'OupsDebuggerSelectionStrategy' }

{ #category : '*System-Settings-Core' }
OupsDebuggerSelectionStrategy class >> debuggerSelectionStrategySettingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #debuggerSelectionStrategy)
		label: 'Debugger selection strategy';
		target: self;
		default: self defaultDebuggerSelectionStrategy;
		parent: #debugging;
		domainValues: (self allSubclasses);
		description: 'Sets the strategy to open a debugger.'
]
