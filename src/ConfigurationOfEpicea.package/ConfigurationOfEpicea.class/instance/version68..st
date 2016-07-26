versions
version68: spec
	<version: '6.8' imports: #('6.6-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Several fixes to undo and redo visitors.
	* Fix in display building from Gustavo (thanks!).
	* Removed some (and last remaining) unnecessary overrides in Pharo.
	* Removed some unused methods in models.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-11-24 23:11'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.187';
			package: 'Epicea' with: 'Epicea-MartinDias.767';
			package: 'EpiceaTests' with: 'EpiceaTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.14';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.17';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
