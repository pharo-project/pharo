versions
version10: spec 
	<version: '1.0' imports: #('1.0-baseline')>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.14';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].
