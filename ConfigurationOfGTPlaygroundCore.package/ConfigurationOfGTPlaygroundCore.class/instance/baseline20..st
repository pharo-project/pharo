baselines
baseline20: spec
	<version: '2.0-baseline'>

	spec for: #'common' do: [
	spec blessing: #'baseline'.
	spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
	
	spec package: 'GT-Playground' with: [
			spec requires: #('GTInspectorCore' ). ].
		
	spec project: 'GTInspectorCore' with: [
			spec
				className: #ConfigurationOfGTInspectorCore;
				versionString: #'development';
				repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
	
	spec group: 'default' with: #('GT-Playground' ) ].