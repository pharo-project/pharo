baselines
baseline: spec
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec
"			project: 'Cargo'
				with: [ spec
					className: #ConfigurationOfCargo;
					versionString: #bleedingEdge;
					loads: #('client');
					repository: 'http://smalltalkhub.com/mc/demarey/CargoPackageManager/main/' ];"
			project: 'Beacon'
				with: [ spec
					className: #ConfigurationOfBeacon;
					versionString: #bleedingEdge;
					repository: 'http://smalltalkhub.com/mc/girba/Beacon/main/' ];
"			baseline: 'OSSubprocess'
				with: [ spec repository: 'github://marianopeck/OSSubprocess:master/repository' ];"
			project: 'Espell'
				with: [ spec
					className: #ConfigurationOfEspell;
					versionString: #bleedingEdge;
					repository: 'http://www.smalltalkhub.com/mc/Guille/ObjectSpace/main' ];
			project: 'Ficus'
				with: [ spec
					className: #ConfigurationOfFicus;
					versionString: '0.3.6';
					loads: #('mini');
					repository: 'http://www.smalltalkhub.com/mc/MartinDias/Ficus/main' ];
			package: 'Pharo30Bootstrap' with: [ spec requires: #('Espell' 'Ficus'" 'Cargo'" 'Beacon') ].
		spec group: 'dev' with: #('Pharo30Bootstrap' 'Espell' 'Ficus') ]