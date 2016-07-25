versions
version49: spec
	<version: '4.9' imports: #('4.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Fixed bug when running tests from Nautilus.'.
		spec author: 'MartinDias'.
		spec timestamp: '2015-04-03 12:20'.
		spec project: 'STON' with: '0.10'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.176';
			package: 'Epicea' with: 'Epicea-MartinDias.681'. ].

	spec for: #'pharo3.0.x' do: [
		spec package: 'EpiceaInPharo30' with: 'EpiceaInPharo30-MartinDias.1'. ].
