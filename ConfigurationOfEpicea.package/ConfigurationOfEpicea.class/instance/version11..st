versions
version11: spec
	<version: '1.1' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec 
			description: 'Visualise comment in nice morphs. Text search is broken. Added some tests.';
			author: 'Martin Dias';
			timestamp: '2013-08-14'.
		
		spec
			package: 'Epicea'  with: 'Epicea-MartinDias.115';
			package: 'Ombu' with: 'Ombu-MartinDias.12';			
			project: 'Phexample' with: #development ].