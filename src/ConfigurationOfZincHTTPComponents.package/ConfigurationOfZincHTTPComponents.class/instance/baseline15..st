baselines
baseline15: spec
	<version: '1.5-baseline'>
	spec
		for: #common
		do: [ 
			spec
				blessing: #baseline;
				description: 'Baseline for Zinc HTTP Components, a framework to deal with the HTTP networking';
				author: 'PaulDeBruicker';
				timestamp: '4/4/2012 09:12';
				repository: 'http://www.squeaksource.com/ZincHTTPComponents'.
			spec package: 'Zodiac-Core' with: [ spec repository: 'http://www.squeaksource.com/Zodiac' ].
			spec
				package: 'Zinc-HTTP';
				package: 'Zinc-Patch-HTTPSocket'
					with: [ 
							spec
								repository: 'http://ss3.gemstone.com/ss/pharo';
								requires: 'Zinc-HTTP' ];
				package: 'Zinc-AWS' with: [ spec requires: 'Zinc-HTTP' ];
				package: 'Zinc-REST' with: [ spec requires: 'Zinc-HTTP' ];
				package: 'Zinc-Zodiac' with: [ spec requires: #('Zinc-HTTP' 'Zodiac-Core') ];
				package: 'Zinc-WebDAV' with: [ spec requires: 'Zinc-HTTP' ];
				package: 'Zinc-WWS-Server' with: [ spec requires: 'Zinc-HTTP' ];
				package: 'Zinc-WWS-Client' with: [ spec requires: 'Zinc-HTTP' ];
				package: 'Zinc-Tests' with: [ spec requires: 'Zinc-HTTP' ].
			spec
				group: 'default' with: #('Core');
				group: 'Core' with: #('Zinc-HTTP');
				group: 'Tests' with: #('Zinc-Tests');
				group: 'AWS' with: #('Zinc-AWS');
				group: 'WebDAV' with: #('Zinc-WebDAV');
				group: 'WWS' with: #('Zinc-WWS-Server' 'Zinc-WWS-Client');
				group: 'REST' with: #('Zinc-REST');
				group: 'Patch-HTTPSocket' with: #('Zinc-Patch-HTTPSocket') ].
	spec
		for: #gemstone
		do: [ 
			spec repository: 'http://seaside.gemstone.com/ss/ZincHTTPComponents'.
			spec
				package: 'Zinc-HTTP' with: [ spec includes: #('Zinc-Gemstone') ];
				package: 'SocketStream' with: [ spec repository: 'http://seaside.gemstone.com/ss/PharoCompat' ];
				package: 'Zinc-GemstonePreliminary' with: [ spec requires: #('SocketStream') ];
				package: 'Zinc-HTTP' with: [ spec requires: #('Zinc-GemstonePreliminary') ];
				package: 'Zinc-Gemstone' with: [ spec requires: #('Zinc-HTTP') ];
				package: 'Zinc-Tests' with: [ spec requires: 'Zinc-Gemstone' ] ]