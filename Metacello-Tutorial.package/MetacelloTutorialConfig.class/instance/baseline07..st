baselines
baseline07: spec 
	<version: '0.7-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Example'.
		
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [ spec requires: 'Example-Core' ] ].
