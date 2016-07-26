baselines
baseline10: spec 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Project'.
		spec
			project: 'Example ALL' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.0';
					loads: #('ALL' );
					file: 'MetacelloTutorialConfig';
					repository: 'http://seaside.gemstone.com/ss/metacello' ].
		spec 
			package: 'Project-Core' with: [ spec requires: 'Example ALL' ];
			package: 'Project-Tests' with: [ spec requires: 'Project-Core' ].].
