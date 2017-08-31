settings
exceptionStrategySettingsOn: aBuilder
	<systemsettings>
	
	(aBuilder pickOne: #current)
		label: 'Exception handling strategy';
		target: self;
		parent: #renraku;
		description: self settingDescription;
		domainValues: (
			self allSubclasses collect: [ :c |
				c strategyTitle -> c ])