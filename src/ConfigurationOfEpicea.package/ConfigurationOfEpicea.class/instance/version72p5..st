versions
version72p5: spec
	<version: '7.2.p5' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Split tests in Ombu.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-12-07 19:42'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.188';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.1';
			package: 'Epicea' with: 'Epicea-MartinDias.773';
			package: 'EpiceaTests' with: 'EpiceaTests-MartinDias.3';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-MartinDias.3';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.1';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
