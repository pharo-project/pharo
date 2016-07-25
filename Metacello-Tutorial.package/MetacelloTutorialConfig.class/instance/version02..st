versions
version02: spec 
	<version: '0.2'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.9';
					repository: 'http://www.example.com/Example' ] ].
