settings
reportServerSettingsOn: aBuilder

	(aBuilder group: #shoreLineServer)
		label: 'Server settings' translated;
		parent: #shoreLine;
		noOrdering.

	(aBuilder setting: #host)
		target: SlReporter;
		label: 'ShoreLine server address' translated;
		description: 'Address of the ShoreLine server' translated;
		parent: #shoreLineServer.

	(aBuilder setting: #port)
		target: SlReporter;
		label: 'ShoreLine server port' translated;
		description: 'Port of the ShoreLine Server' translated;
		parent: #shoreLineServer.