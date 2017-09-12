baselines
baseline: spec
	<baseline>
	spec for: #common do: [ spec

		project: 'Ficus' with: [
			spec
				className: #ConfigurationOfFicus;
				versionString: '0.3.7';
				loads: #FicusRing;
				repository: 'http://smalltalkhub.com/mc/MartinDias/Ficus/main' ];

		package: 'Hermes';
		package: 'Hermes-Tests-External-Package';
		package: 'Hermes-Ring' with: [ spec requires: #('core' 'Ficus') ];
		package: 'Hermes-Tests' with: [ spec requires: #('core' 'Hermes-Tests-External-Package') ];

		group: 'core'
			with: #(Hermes);
		group: 'ring'
			with: #('Hermes-Ring');
		group: 'test'
			with: #('Hermes-Tests' 'Hermes-Tests-External-Package')
	]