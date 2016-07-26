versions
version31: spec
	<version: '3.1' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Added "any code change" filter.
			- Now only EpEntryItems in the selection are taken into account for menu actions.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '03/03/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.129';
			package: 'Epicea' with: 'Epicea-MartinDias.529'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].