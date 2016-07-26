versions
version56: spec
	<version: '5.6' imports: #('5.4-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Fix wrong use of FileLocator>>path.
	* Working in both Pharo 40 and current Pharo 50 (50182).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-22 15:20'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-MartinDias.708';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.11';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
