settings
reportSettingsOn: aBuilder
	<systemsettings>

	(aBuilder group: #shoreLine)
		parent: #privacy;
		label: 'ShoreLine Reporter' translated;
		noOrdering.

	self reportFlagSettingsOn: aBuilder.
	self reportServerSettingsOn: aBuilder.