versions
version59: spec
	<version: '5.9' imports: #('5.9-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Changed initial ombu files directory to "<preferences>/epicea-sessions"
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-09-09 21:59'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-MartinDias.720';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-VincentBlondeau.12';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
