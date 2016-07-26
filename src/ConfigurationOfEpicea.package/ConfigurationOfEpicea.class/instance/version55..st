versions
version55: spec
	<version: '5.5' imports: #('5.4-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Fix Redo and Undo operations in Pharo50.
	* Minor fix in item colors.
	* Working in both Pharo 40 and current Pharo 50 (50177).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-20 17:36'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.180';
			package: 'Epicea' with: 'Epicea-MartinDias.707';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.11';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
