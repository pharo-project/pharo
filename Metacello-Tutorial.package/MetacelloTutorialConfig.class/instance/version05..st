versions
version05: spec 
	<version: '0.5'>
	
	spec for: #common do: [
		spec repository: 'http://www.example.com/Example'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.11';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1' ].
