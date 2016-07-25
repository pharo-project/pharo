versions
version44: spec
	<version: '4.4' imports: #('4.1-baseline') >

	spec for: #'common' do: [
		spec description: '
	* Mainly some cosmetic improvements
'.
		spec author: 'MartinDias'.
		spec timestamp: '21/08/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.163';
			package: 'Epicea' with: 'Epicea-MartinDias.632'. ].