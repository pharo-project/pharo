versions
version62: spec
	<version: '6.2' imports: #('6.1-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Minor update: Implemented #visitCodeChange: in EpTestVisitor because ReleaseTest doesn''t like it else.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-09-28 21:45'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-MartinDias.738';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-SkipLentz.16';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
