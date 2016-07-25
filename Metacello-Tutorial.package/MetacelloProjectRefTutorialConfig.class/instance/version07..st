versions
version07: spec 
	<version: '0.7' imports: #('0.7-baseline' )>
	
	spec for: #common do: [
		spec blessing: #release.
		
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1';
			package: 'Example-Core' with: 'Example-Core-anon.12';
			package: 'Example-Tests' with: 'Example-Tests-anon.3';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1'.].
