versions
version45: spec
	<version: '4.5' imports: #('4.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Working in Pharo 30859 and 40330'.
		spec author: 'MartinDias'.
		spec timestamp: '10/27/2014 17:12'.
		spec project: 'STON' with: '0.3-baseline'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.166';
			package: 'Epicea' with: 'Epicea-MartinDias.644'. ].

	spec for: #'pharo3.0.x' do: [
		spec package: 'EpiceaInPharo30' with: 'EpiceaInPharo30-MartinDias.1'. ].
