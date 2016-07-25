versions
version10: spec
	<version: '1.0' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec 
			description: 'First public version, migrated from sandbox repository.';
			author: 'Martin Dias';
			timestamp: '2013-08-13'.
		
		spec
			package: 'Epicea'  with: 'Epicea-MartinDias.94';
			package: 'Ombu' with: 'Ombu-MartinDias.12';			
			project: 'Phexample' with: #development ].