versions
version08: spec 
	<version: '0.8' imports: #('0.7-baseline')>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.15';
			package: 'Example-Tests' with: 'Example-Tests-anon.6';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.2' ].
