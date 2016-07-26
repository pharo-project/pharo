settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #handMorphsLogEventStats)
		parent: #pharoSystem;
		label: 'HandMorph event statistics';
		target: self;
		selector: #logEventStatsEnabled;
		description: 'Enable/disable gathering global statistics of events handled by HandMorphs.
"HandMorph eventStats inspect"'
