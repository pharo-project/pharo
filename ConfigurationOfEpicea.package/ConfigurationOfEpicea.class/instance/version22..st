versions
version22: spec
	<version: '2.2' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			Fixed bugs with refactorings.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '11/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.125';
			package: 'Epicea' with: 'Epicea-MartinDias.488'.
			
		spec
			preLoadDoIt: #version20PreLoad;
			postLoadDoIt: #version21PostLoad ].