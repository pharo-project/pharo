settings-definition
inspectorPlugginSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #inspectorPluggin)
		order: 5; 
		target: self;
		label: 'Inspector Plugin';
		description: 'Enable GTInspector plugin which shows a tab with number of critiques and allows to inspect them.';
		parent: #renraku.