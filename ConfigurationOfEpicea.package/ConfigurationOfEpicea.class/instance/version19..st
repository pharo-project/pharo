versions
version19: spec
	<version: '1.9' imports: #('1.4-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Some fixes
			- Working in Pharo #30695.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '14/01/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.124';
			package: 'Epicea' with: 'Epicea-MartinDias.469'. ].