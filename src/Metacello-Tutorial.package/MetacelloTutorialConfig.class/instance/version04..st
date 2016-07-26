versions
version04: spec 
	<version: '0.4'>
	
	spec for: #common do: [
		spec repository: 'http://www.example.com/Example'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.10';
			package: 'Example-Tests' with: 'Example-Tests-anon.3' ].
