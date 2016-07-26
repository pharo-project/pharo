versions
version06: spec 
	<version: '0.6'>
	
	spec for: #common do: [
		spec repository: 'http://www.example.com/Example'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.12';
			package: 'Example-Tests' with: [
				spec 
					file: 'Example-Tests-anon.3';
					requires: 'Example-Core' ];
			package: 'Example-AddOn' with: [
				spec 
					file: 'Example-AddOn-anon.1';
					requires: 'Example-Core' ]].
