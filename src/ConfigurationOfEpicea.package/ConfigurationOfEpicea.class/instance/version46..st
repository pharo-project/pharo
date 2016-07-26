versions
version46: spec
	<version: '4.6' imports: #('4.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Prepared for integration into Pharo 4'.
		spec author: 'MartinDias'.
		spec timestamp: '12/12/2014 17:12'.
		spec project: 'STON' with: '0.10'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.171';
			package: 'Epicea' with: 'Epicea-MartinDias.670'. ].

	spec for: #'pharo3.0.x' do: [
		spec package: 'EpiceaInPharo30' with: 'EpiceaInPharo30-MartinDias.1'. ].
