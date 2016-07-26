versions
version57: spec
	<version: '5.7' imports: #('5.4-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Fix DNU in Pharo 50265. No need to register to FileServices, so remove methods.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-08-24 16:06'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-MartinDias.709';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-VincentBlondeau.12';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
