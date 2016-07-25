versions
version37: spec
	<version: '3.7' imports: #('3.3-baseline') >

	spec for: #'common' do: [
		spec description: ''.
		spec author: 'MartinDias'.
		spec timestamp: '23/04/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.154';
			package: 'Epicea' with: 'Epicea-MartinDias.583'. ].