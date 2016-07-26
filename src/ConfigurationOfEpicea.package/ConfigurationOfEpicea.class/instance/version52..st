versions
version52: spec
	<version: '5.2' imports: #('5.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Version 5.1 was not logging MC save events in Pharo 50.
	* Working in both Pharo 40 and current Pharo 50 (50162).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-09 11:45'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.180';
			package: 'Epicea' with: 'Epicea-MartinDias.706';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.9'. ].
