versions
version43: spec
	<version: '4.3' imports: #('4.1-baseline') >

	spec for: #'common' do: [
		spec description: '
	* Removing uses of RGFactory since it is being deprecated in Pharo 4. 
'.
		spec author: 'MartinDias'.
		spec timestamp: '13/08/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.163';
			package: 'Epicea' with: 'Epicea-MartinDias.628'. ].