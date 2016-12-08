baselines
baseline: spec
	<baseline>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec
			project: 'Cargo'
				with: [ spec
					className: #ConfigurationOfCargo;
					versionString: #bleedingEdge;
					loads: #('client');
					repository: 'http://smalltalkhub.com/mc/demarey/CargoPackageManager/main/' ];
			project: 'Espell'
				with: [ spec
					className: #ConfigurationOfEspell;
					versionString: #bleedingEdge;
					repository: 'http://www.smalltalkhub.com/mc/Guille/ObjectSpace/main' ];
			project: 'Ficus'
				with: [ spec
					className: #ConfigurationOfFicus;
					versionString: '0.3.5';
					loads: #('mini');
					repository: 'http://www.smalltalkhub.com/mc/MartinDias/Ficus/main' ].

			spec
				baseline: 'PharoBootstrap' with: [
					spec repository: 'filetree://src' ];
				package: 'Pharo30Bootstrap' with: [ spec requires: #('Espell' 'Ficus' 'Cargo') ].
			spec group: 'dev' with: #('Pharo30Bootstrap' 'Espell' 'Ficus') ]