versions
version792p6: spec
	<version: '7.92.p6' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Bug fix: auto detection lost changes should not fail when epicea directory is not exists
'.
	
		spec author: 'PavelKrivanek'.
		spec timestamp: '2016-06-17 17:00'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-DenisKudryashov.197';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.4';
			package: 'Epicea' with: 'Epicea-DenisKudryashov.787';
			package: 'EpiceaTests' with: 'EpiceaTests-MD.6';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-TheIntegrator.17';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
