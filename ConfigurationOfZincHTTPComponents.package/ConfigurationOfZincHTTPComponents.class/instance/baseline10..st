baselines
baseline10: spec 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec 
			blessing: #baseline;
			description: 'Baseline for Zinc HTTP Components, a framework to deal with the HTTP networking';
			author: 'SvenVanCaekenberghe';
			repository: 'http://www.squeaksource.com/ZincHTTPComponents'.	
		spec project: 'Seaside30' with: [
				spec
					className: 'ConfigurationOfSeaside30';
					repository: 'http://www.squeaksource.com/MetacelloRepository' ].
		spec 
			package: 'Zinc-HTTP';
			package: 'Zinc-Patch-HTTPSocket' with: [ spec requires: 'Zinc-HTTP' ];
			package: 'Zinc-AWS' with: [ spec requires: 'Zinc-HTTP' ];
			package: 'Zinc-Tests' with: [ spec requires: 'Zinc-HTTP' ];
			package: 'Zinc-Seaside' with: [ spec requires: #('Zinc-HTTP' 'Seaside30') ].
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('Zinc-HTTP');
			group: 'Tests' with: #('Zinc-Tests');
			group: 'Seaside' with: #('Zinc-Seaside');
			group: 'Patch-HTTPSocket' with: #('Zinc-Patch-HTTPSocket') ].
		
	spec for: #gemstone do:[
		spec repository: 'http://seaside.gemstone.com/ss/ZincHTTPComponents'.  
		spec
			package:'SocketStream' with:[spec repository: 'http://seaside.gemstone.com/ss/PharoCompat' ];
			package:'Seaside-GemStone-Adaptors-Zinc' with:[spec requires: #('Zinc-Gemstone' 'Zinc-Seaside')] ;
			package:'Zinc-GemstonePreliminary' with:[spec requires: #('SocketStream')  ];
			package:'Zinc-HTTP' with:[spec requires: #('Zinc-GemstonePreliminary')  ];
			package:'Zinc-Gemstone' with:[spec requires: #('Zinc-HTTP')  ];
			package: 'Zinc-Tests' with: [ spec requires: 'Zinc-Gemstone' ].
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('Zinc-Gemstone');
			group: 'Tests' with: #('Zinc-Tests');
			group: 'Seaside' with: #('Seaside-GemStone-Adaptors-Zinc').
		
		]