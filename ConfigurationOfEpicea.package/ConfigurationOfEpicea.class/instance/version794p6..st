versions
version794p6: spec
	<version: '7.94.p6' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Fogbugz issue 18383
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2016-06-24 11:21'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-DenisKudryashov.201';
			package: 'OmbuTests' with: 'OmbuTests-DenisKudryashov.6';
			package: 'Epicea' with: 'Epicea-DenisKudryashov.787';
			package: 'EpiceaTests' with: 'EpiceaTests-MD.6';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-MartinDias.18';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-TheIntegrator.3';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
