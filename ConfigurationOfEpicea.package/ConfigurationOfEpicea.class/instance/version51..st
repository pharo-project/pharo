versions
version51: spec
	<version: '5.1' imports: #('5.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Working in both Pharo 40 and current Pharo 50 (50161)
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-08 18:37'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.180';
			package: 'Epicea' with: 'Epicea-MartinDias.704';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.8'. ].
