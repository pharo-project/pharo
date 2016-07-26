versions
version34: spec
	<version: '3.4' imports: #('3.3-baseline') >

	spec for: #'common' do: [
		spec description: ''.
		spec author: 'MartinDias'.
		spec timestamp: '16/04/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.153';
			package: 'Epicea' with: 'Epicea-MartinDias.578'. ].