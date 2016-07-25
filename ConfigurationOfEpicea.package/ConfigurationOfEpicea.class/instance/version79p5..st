versions
version79p5: spec
	<version: '7.9.p5' imports: #('7.2.p5-baseline')>

	spec for: #'pharo5.x' do: [
		spec blessing: #'release'.
		spec description: '
	* EpLog browse renamed to browseEvents to not hide usual meaning of browse.
	* Many usefull methods on EpMonitor class side to retrive logs from file directory.
	* EpLostChangesDetector to opening lost changes log on image startup
'.
	
		spec author: 'DenisKudryashov'.
		spec timestamp: '2016-05-30 10:47'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-DenisKudryashov.197';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.4';
			package: 'Epicea' with: 'Epicea-DenisKudryashov.784';
			package: 'EpiceaTests' with: 'EpiceaTests-MD.6';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-DenisKudryashov.13';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
