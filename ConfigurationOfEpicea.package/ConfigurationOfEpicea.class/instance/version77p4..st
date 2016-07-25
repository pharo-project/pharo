versions
version77p4: spec
	<version: '7.7.p4' imports: #('7.2.p4-baseline')>

	spec for: #'pharo4.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Patch for Pharo 4.0: there was a problem to register a change in class comment.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2016-02-17 23:14'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.192';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.3';
			package: 'Epicea' with: 'Epicea-MartinDias.776';
			package: 'EpiceaTests' with: 'EpiceaTests-MD.6';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-MartinDias.10';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.16';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
