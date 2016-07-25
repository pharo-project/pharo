versions
version61: spec
	<version: '6.1' imports: #('6.1-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Add tests for redo and undo functionality, currently disabled.
	* Remove calls to deprecated Announcer messages.
'.
	
		spec author: 'SkipLentz'.
		spec timestamp: '2015-09-28 15:12'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-SkipLentz.737';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-SkipLentz.16';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
