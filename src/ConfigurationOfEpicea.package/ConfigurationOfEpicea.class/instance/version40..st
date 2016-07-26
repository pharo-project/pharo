versions
version40: spec
	<version: '4.0' imports: #('3.3-baseline') >

	spec for: #'common' do: [
		spec description: '
	* Session browser: sort by clicking on columns
	* OmFileStores are lazy.
'.
		spec author: 'MartinDias'.
		spec timestamp: '05/06/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.161';
			package: 'Epicea' with: 'Epicea-MartinDias.617'. ].