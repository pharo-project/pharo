versions
version03: spec 
	<version: '0.3'>
	
	spec for: #common do: [
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.10';
					repository: 'http://www.example.com/Example' ];
			package: 'Example-Tests' with: [
				spec 
					file: 'Example-Tests-anon.3';
					repository: 'http://www.example.com/Example' ]].
