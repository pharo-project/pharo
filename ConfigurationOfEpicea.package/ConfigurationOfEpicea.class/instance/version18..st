versions
version18: spec
	<version: '1.8' imports: #('1.4-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Created File Reader Service for ChangeSet importer.
			- Working in Pharo #30572.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '14/11/2013'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.120';
			package: 'Epicea' with: 'Epicea-MartinDias.425'. ].