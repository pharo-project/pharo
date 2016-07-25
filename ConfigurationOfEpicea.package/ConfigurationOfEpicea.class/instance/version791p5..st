versions
version791p5: spec
	<version: '7.91.p5' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Bug fix: auto detection lost changes should not fail when epicea directory is not exists
'.
	
		spec author: 'DenisKudryashov'.
		spec timestamp: '2016-05-31 14:19'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-DenisKudryashov.197';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.4';
			package: 'Epicea' with: 'Epicea-DenisKudryashov.785';
			package: 'EpiceaTests' with: 'EpiceaTests-MD.6';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-DenisKudryashov.13';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
