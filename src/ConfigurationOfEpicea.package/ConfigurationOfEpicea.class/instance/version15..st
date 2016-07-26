versions
version15: spec
	<version: '1.5' imports: #('1.4-baseline') >

	spec for: #'common' do: [
		spec description: 'Just fixed Undo and Redo operations of previous version.'.
		spec author: 'MartinDias'.
		spec timestamp: '24/10/2013'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.110';
			package: 'Epicea' with: 'Epicea-MartinDias.330'. ].