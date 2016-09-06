settings-definition
spotterPluginSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #spotterPlugin)
		order: 5;
		target: self;
		label: 'Spotter Plugin' translated;
		description: 'Enable GTSpotter plugin which shows a group with critiques and allows to preview them.';
		default: false;
		parent: #renraku.