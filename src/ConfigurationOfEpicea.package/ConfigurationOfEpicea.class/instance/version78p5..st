versions
version78p5: spec
	<version: '7.8.p5' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Use Smalltalk session instead of Session current, since the class was removed in Pharo 5.
	* Also, load latest stable Ston in Pharo 4 (Pharo 5 does not load any).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2016-03-16 14:45'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-VincentBlondeau.193';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.3';
			package: 'Epicea' with: 'Epicea-MartinDias.776';
			package: 'EpiceaTests' with: 'EpiceaTests-MD.6';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-MartinDias.10';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
