versions
version16: spec
	<version: '1.6' imports: #('1.4-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Added icons to contextual menu.
			- Some internal refactorings.
			- Some minor fixes.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '25/10/2013'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.110';
			package: 'Epicea' with: 'Epicea-MartinDias.338'. ].