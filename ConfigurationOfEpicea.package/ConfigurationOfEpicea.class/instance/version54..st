versions
version54: spec
	<version: '5.4' imports: #('5.4-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Set recording directory to user preferences directory, as it was in previous versions.
	* Working in both Pharo 40 and current Pharo 50 (50170).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-14 09:55'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.180';
			package: 'Epicea' with: 'Epicea-MartinDias.706';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.10';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
