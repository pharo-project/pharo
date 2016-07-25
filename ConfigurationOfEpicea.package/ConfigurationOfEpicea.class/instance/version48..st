versions
version48: spec
	<version: '4.8' imports: #('4.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'release'.
		spec description: '
	* Added a #triggering group.
	* Workaround for tolerating OS changes in file references.
	* Fixed "Browse" for absent methods.'.
		spec author: 'MartinDias'.
		spec timestamp: '2015-01-13 19:32'.
		spec project: 'STON' with: '0.10'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.175';
			package: 'Epicea' with: 'Epicea-MartinDias.678'. ].

	spec for: #'pharo3.0.x' do: [
		spec package: 'EpiceaInPharo30' with: 'EpiceaInPharo30-MartinDias.1'. ].
