versions
version10: spec 
	<version: '1.0' imports: #('1.0-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1'.].
