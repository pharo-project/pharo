versions
version69: spec
	<version: '6.9' imports: #('6.6-baseline')>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Add #asEpiceaRingDefinition to TClass as explicit requirement, since it was defined in Class and in Trait. This is a requirement in Pharo 5.
'.
	
		spec author: 'MartinDias'.
		spec timestamp: '2015-11-25 22:40'.
		spec project: 'STON' with: '0.14'.
		spec project: 'SMark' with: '1.0.1'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.187';
			package: 'Epicea' with: 'Epicea-MD.768';
			package: 'EpiceaTests' with: 'EpiceaTests-MartinDias.2';
			package: 'Hiedra' with: 'Hiedra-MartinDias.136';
			package: 'EpiceaHiedra' with: 'EpiceaHiedra-MD.11';
			package: 'EpiceaBenchmarks' with: 'EpiceaBenchmarks-MartinDias.4';
			package: 'EpiceaInPharo40' with: 'EpiceaInPharo40-MartinDias.14';
			package: 'EpiceaInPharo50' with: 'EpiceaInPharo50-MartinDias.17';
			package: 'EpiceaKomitter' with: 'EpiceaKomitter-MartinDias.13';
			package: 'EpiceaSUnit' with: 'EpiceaSUnit-MartinDias.4'. ].
