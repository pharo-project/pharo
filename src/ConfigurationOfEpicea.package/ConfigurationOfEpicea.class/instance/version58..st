versions
version58: spec
	<version: '5.8' imports: #('5.4-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* You need to register to FileServices in Pharo 4.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-08-24 16:06'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-MartinDias.709';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-VincentBlondeau.12';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
