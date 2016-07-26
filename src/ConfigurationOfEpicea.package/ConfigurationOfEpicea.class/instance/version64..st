versions
version64: spec
	<version: '6.4' imports: #('6.4-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Adding benchmarks group.
	* Enhancing World menu entries
	* Fixed some undo cases.
	* Updated test to do not load external project but a mock.
	* Migrated to Pharo 5 (some deprecated method).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-10-22 15:28'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.187';
			package: 'Epicea' with: 'Epicea-MartinDias.755';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-SkipLentz.16';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
