baselines
baseline11: spec 
	<version: '1.1-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.example.com/Project'.

		spec
			project: 'Example Default' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.1';
					loads: #('default' );
					file: 'Metacello-Tutorial';
					repository: 'http://seaside.gemstone.com/ss/metacello' ];
			project: 'Example Tests' with: [
				spec
					className: 'MetacelloTutorialConfig';
					versionString: '1.1';
					loads: #('Tests' );
					file: 'Metacello-Tutorial';
					repository: 'http://seaside.gemstone.com/ss/metacello' ].
		spec 
			package: 'Project-Core' with: [ spec requires: 'Example Default' ];
			package: 'Project-Tests' with: [ 
				spec requires: #('Project-Core' 'Example Tests' ).].].
