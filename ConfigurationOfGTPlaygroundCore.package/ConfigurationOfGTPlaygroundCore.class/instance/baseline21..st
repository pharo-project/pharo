baselines
baseline21: spec
	<version: '2.1-baseline'>

	spec for: #'common' do: [
	spec blessing: #'baseline'.
	spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
	
	spec package: 'GT-Playground' with: [
			spec requires: #('GTInspectorCore')].
	spec package: 'GT-Tests-Playground' with: [
			spec requires: #('GT-Playground')].
		
	spec project: 'GTInspectorCore' with: [
			spec
				className: #ConfigurationOfGTInspectorCore;
				versionString: #'development';
				repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
	
	spec group: 'default' with: #('GT-Playground' 'GT-Tests-Playground') ].