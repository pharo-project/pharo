versions
version53: spec
	<version: '5.3' imports: #('5.1-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Workaround for RewriteTool.
	* Working in both Pharo 40 and current Pharo 50 (50162).
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-07-10 00:44'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.180';
			package: 'Epicea' with: 'Epicea-MartinDias.706';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.11';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.10';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
