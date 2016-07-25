baselines
baseline010: spec
	<version: '0.1.0-baseline'>

	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: (MCSmalltalkhubRepository owner: 'dalsat' project: 'ShoreLine-Reporter') location.
		
		spec
			package: 'ShoreLine-Report-Core';
			package: 'ShoreLine-Report-UI' with: [ spec requires: 'ShoreLine-Report-Core' ];
			package: 'ShoreLine-Report-Settings' with: [ spec requires: 'ShoreLine-Report-Core' ];
			package: 'ShoreLine-Report-Staging' with: [ spec requires: 'ShoreLine-Report-Core' ].
	
		spec
			group: 'default' with: #('UI' 'Settings');
			group: 'Core' with: #('ShoreLine-Report-Core');
			group: 'UI' with: #('Core' 'ShoreLine-Report-UI');
			group: 'Settings' with: #('Core' 'ShoreLine-Report-Settings');
			group: 'Staging' with: #('Core' 'ShoreLine-Report-Staging') ].