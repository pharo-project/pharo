versions
version75p5: spec
	<version: '7.5.p5' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Create random sufixes to test ombu files (required in Windows)
	* Use #closeForWriting on file store (needed by some tests to run in Windows)
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2016-02-02 12:41'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.190';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.2';
			package: 'Epicea' with: 'Epicea-MartinDias.776';
			package: 'EpiceaTests' with: 'EpiceaTests-SkipLentz.5';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-MartinDias.10';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
