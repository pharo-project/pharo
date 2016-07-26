versions
version73p4: spec
	<version: '7.3.p4' imports: #('7.2.p4-baseline')>

	spec for: #'pharo4.x' do: [
		spec blessing: #'release'.
		spec description: '
	* Remove repeated trait methods in users (bug in refactoring tools).
	* Add EpMonticelloVersionsLoad>>{=,hash} for comparing version load events.
	* New enumeration methods in EpLog.
	* Properly dispose of EpLogTestResource in EpLogTest.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2016-01-29 02:04'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.188';
			package: 'OmbuTests' with: 'OmbuTests-MartinDias.1';
			package: 'Epicea' with: 'Epicea-SkipLentz.775';
			package: 'EpiceaTests' with: 'EpiceaTests-SkipLentz.5';
			package: 'EpiceaBrowsers' with: 'EpiceaBrowsers-MartinDias.4';
			package: 'EpiceaBrowsersTests' with: 'EpiceaBrowsersTests-MartinDias.2';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.15';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
