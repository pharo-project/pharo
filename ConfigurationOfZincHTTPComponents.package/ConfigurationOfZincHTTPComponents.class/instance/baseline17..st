baselines
baseline17: spec
	<version: '1.7-baseline'>
	spec
		for: #common
		do: [ 
			spec
				blessing: #baseline;
				description: '
- add XMLSupport project
- update Zinc-AWS to require above
- add  FileSystem-Legacy to Pharo 1.x and FileSystem to Pharo 2.x';
				author: 'SeanDeNigris';
				timestamp: '7/4/2012 12:12-04:00';
				repository: 'http://www.squeaksource.com/ZincHTTPComponents'.
			spec project: 'XML Support' with: [
				spec
					className: 'ConfigurationOfXMLSupport';
					versionString: #'stable';
					repository: 'http://www.squeaksource.com/MetacelloRepository' ].
			spec package: 'Zodiac-Core' with: [ spec repository: 'http://www.squeaksource.com/Zodiac' ].
			spec
				package: 'Zinc-HTTP';
				package: 'Zinc-Patch-HTTPSocket'
					with: [ 
							spec
								repository: 'http://ss3.gemstone.com/ss/pharo';
								requires: 'Zinc-HTTP' ];
				package: 'Zinc-AWS' with: [ spec requires: #('Zinc-HTTP' 'XML Support') ];
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
	spec for: #'pharo1.x' do: [ 
		spec
			package: 'Zinc-FileSystem-Legacy';
			package: 'Zinc-HTTP' with: [ spec requires: 'Zinc-FileSystem-Legacy' ] ].
	spec for: #'pharo2.x' do: [
		spec
			package: 'Zinc-FileSystem';
			package: 'Zinc-HTTP' with: [ spec requires: 'Zinc-FileSystem' ] ].
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