baseline
baseline: spec
	<baseline>

	spec for: #common do: [
		spec 
			package: 'SUnit-Core';
			package: 'SUnit-Tests';
			package: 'SUnit-UI';
			package: 'SUnit-UITesting';
			package: 'SUnit-Help';
			package: 'HudsonBuildTools20'.
		spec 
			group: 'Core' with: #('SUnit-Core');
			group: 'UI' with: #('SUnit-UI' 'SUnit-UITesting' );
			group: 'Tests' with: #('SUnit-Tests' );
			group: 'Help' with: #('SUnit-Help' );
			group: 'JenkinsSupport' with: #('HudsonBuildTools20' );
			group: 'default' with: #('SUnit-Core' 'SUnit-Tests' 'JenkinsSupport' ). ].		
