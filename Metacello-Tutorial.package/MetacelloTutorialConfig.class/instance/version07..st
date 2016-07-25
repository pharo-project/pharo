versions
version07: spec 
	<version: '0.7' imports: #('0.7-baseline')>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.12';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1' ].
