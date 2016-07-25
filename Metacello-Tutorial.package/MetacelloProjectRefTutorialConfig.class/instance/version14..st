versions
version14: spec 
	<version: '1.4' imports: #('1.4-baseline' )>
	
	spec for: #common do: [
		spec blessing: #beta.
		spec description: 'Add groups and Project-Extra'.

		spec
			project: 'Example Default' with: '1.3';
			project: 'Example Tests' with: '1.3'.
		spec 
			package: 'Project-Core' with: 'Project-Core-anon.2';
			package: 'Project-Tests' with: 'Project-Tests-anon.2';
			package: 'Project-Extra' with: 'Project-Extra-anon.1'.].
