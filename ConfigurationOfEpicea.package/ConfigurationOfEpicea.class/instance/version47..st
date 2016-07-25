versions
version47: spec
	<version: '4.7' imports: #('4.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Removed overrides to system packages (buh).
	* Prepared for integration into Pharo 4.'.
		spec author: 'MartinDias'.
		spec timestamp: '15/12/2014 18:12'.
		spec project: 'STON' with: '0.10'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.173';
			package: 'Epicea' with: 'Epicea-MartinDias.676'. ].

	spec for: #'pharo3.0.x' do: [
		spec package: 'EpiceaInPharo30' with: 'EpiceaInPharo30-MartinDias.1'. ].
