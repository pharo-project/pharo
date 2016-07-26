versions
version60: spec
	<version: '6.0' imports: #('6.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Add EpCategoryRename event. Update visitors accordingly.
	* Fix override to parseNextDeclaration.
	* Remove overridden method "MCVersionLoader>>load", it has since been added to MCVersionLoader and is not needed anymore.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-09-15 18:14'.
		spec project: 'STON' with: '0.14'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.181';
			package: 'Epicea' with: 'Epicea-SkipLentz.722';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.12';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.15';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13'. ].
