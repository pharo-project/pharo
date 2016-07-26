versions
version65: spec
	<version: '6.5' imports: #('6.5-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Split SUnit support from core package to an optional one (EpiceaSUnit).
	* Fixed World menu entries for Pharo 40.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-11-09 21:54'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.187';
			package: 'Epicea' with: 'Epicea-MartinDias.759';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.14';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-SkipLentz.16';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
