versions
version23: spec
	<version: '2.3' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			Same code as 2.2, but safer for upgrading from previous versions.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '12/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.125';
			package: 'Epicea' with: 'Epicea-MartinDias.488'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].