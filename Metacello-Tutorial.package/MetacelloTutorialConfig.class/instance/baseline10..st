baselines
baseline10: spec 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Example'.
		
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOnTests' with: [ 
				spec requires: #('Example-AddOn' 'Example-Tests' ) ].
		spec 
			group: 'default' with: #('Example-Core' 'Example-AddOn' );
			group: 'Tests' with: #('Example-Tests' 'Example-AddOnTests' ) ].
