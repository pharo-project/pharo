settings
enableValidateSetting: aBuilder
	<systemsettings>
	(aBuilder setting: #validateBeforeSaving)
		label: 'Validate before commit';
		parent: #versionner;
		target: self;
		description: 'When selected the project will be validated (i.e., the metacello rules will be checked)'