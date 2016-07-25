versions
version63: spec
	<version: '6.3' imports: #('6.1-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Clean up with Code Critics.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-09-28 22:37'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.183';
			package: 'Epicea' with: 'Epicea-MartinDias.742';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-SkipLentz.16';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
