versions
version67: spec
	<version: '6.7' imports: #('6.6-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Experiment: disable redo and undo tests because they seem to cause a problem during integration.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-11-12 18:44'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.187';
			package: 'Epicea' with: 'Epicea-SkipLentz.761';
			package: 'EpiceaTests' with: 'EpiceaTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.14';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-SkipLentz.16';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
