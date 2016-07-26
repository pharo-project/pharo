versions
version21: spec
	<version: '2.1' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			Like 2.0 but working better with Komitter.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '11/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.125';
			package: 'Epicea' with: 'Epicea-MartinDias.485'.
			
		spec
			preLoadDoIt: #version20PreLoad;
			postLoadDoIt: #version21PostLoad ].