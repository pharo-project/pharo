versions
version17: spec
	<version: '1.7' imports: #('1.4-baseline') >

	spec for: #'common' do: [
		spec description: '
			- ChangeSet importer.
			- Working in Pharo #30570.
				- Migrated to new Spec.
				- Some extensions were integrated.
			- More tests.
			- General clean up.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '13/11/2013'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.120';
			package: 'Epicea' with: 'Epicea-MartinDias.417'. ].