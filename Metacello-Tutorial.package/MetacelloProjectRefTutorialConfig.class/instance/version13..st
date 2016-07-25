versions
version13: spec 
	<version: '1.3' imports: #('1.2-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		spec description: 'Import functionality from the example project, different packages'.

		spec 
			package: 'Project-Core' with: 'Project-Core-anon.1';
			package: 'Project-Tests' with: 'Project-Tests-anon.1';
			project: 'Example Default' with: '1.3';
			project: 'Example Tests' with: '1.3'].
