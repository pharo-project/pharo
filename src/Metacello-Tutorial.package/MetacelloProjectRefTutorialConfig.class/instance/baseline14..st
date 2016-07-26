baselines
baseline14: spec 
	<version: '1.4-baseline' imports: #('1.2-baseline' )>
	
	spec for: #common do: [
		spec description: 'Add groups and Project-Extra, extending 1.2-baseline'.
		spec 
			package: 'Project-Core' with: [ spec includes: 'Project-Extra' ];
			package: 'Project-Extra' with: [ spec requires: 'Project-Core' ].
		spec 
			group: 'default' with: #('Project-Core' 'Project-Extra' );
			group: 'Core' with: #('default' );
			group: 'Tests' with: #('Project-Tests' ).].
