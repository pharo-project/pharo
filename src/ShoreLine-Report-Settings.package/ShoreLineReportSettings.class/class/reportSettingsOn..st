settings
reportSettingsOn: aBuilder
	<systemsettings>

	(aBuilder group: #shoreLine)
		parent: #privacy;
		label: 'ShoreLine Reporter';
		noOrdering.

	self reportFlagSettingsOn: aBuilder.
	self reportServerSettingsOn: aBuilder.