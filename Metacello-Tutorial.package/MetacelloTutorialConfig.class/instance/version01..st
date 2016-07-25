versions
version01: spec 
	<version: '0.1'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.8';
					repository: 'http://www.example.com/Example' ] ].
