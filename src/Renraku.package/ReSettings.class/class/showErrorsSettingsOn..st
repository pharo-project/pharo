settings-definition
showErrorsSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #showErrors)
		target: self;
		order: 5;
		label: 'Show errors (dev. only!)';
		description: 'Signal errors that happen during critique-checking (and related). This should be used by developers.';
		parent: #renraku.