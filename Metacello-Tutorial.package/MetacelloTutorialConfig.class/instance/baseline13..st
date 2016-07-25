baselines
baseline13: spec 
	<version: '1.3-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Example'.
		
		spec 
			package: 'Example-Core' with: [
				spec includes: #('Example-Platform' ) ];
			package: 'Example-Tests' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [ spec requires: 'Example-Core' ];
			package: 'Example-Platform' with: [ spec requires: 'Example-Core' ];
			package: 'Example-AddOnTests' with: [ 
				spec requires: #('Example-AddOn' 'Example-Tests' ) ].
		spec 
			group: 'default' with: #('Example-Core' 'Example-AddOn' );
			group: 'Tests' with: #('Example-Tests' 'Example-AddOnTests' ) ].
	spec for: #gemstone do: [
		spec package: 'Example-Platform' with: 'Example-Platform.gemstone'.].
	spec for: #pharo do: [
		spec package: 'Example-Platform' with: 'Example-Platform.pharo'.].
	spec for: #squeak do: [
		spec package: 'Example-Platform' with: 'Example-Platform.squeak'.].