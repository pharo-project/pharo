settings
reportServerSettingsOn: aBuilder

	(aBuilder group: #shoreLineServer)
		label: 'Server settings';
		parent: #shoreLine;
		noOrdering.

	(aBuilder setting: #host)
		target: SlReporter;
		label: 'ShoreLine server address' ;
		description: 'Address of the ShoreLine server';
		parent: #shoreLineServer.

	(aBuilder setting: #port)
		target: SlReporter;
		label: 'ShoreLine server port';
		description: 'Port of the ShoreLine Server';
		parent: #shoreLineServer.