baselines
baseline30: spec
	<version: '3.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		
		spec project: 'GTPlaygroundCore' with: [
				spec
					className: #ConfigurationOfGTPlaygroundCore;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
		
		spec project: 'GTInspectorCore' with: [
				spec
					className: #ConfigurationOfGTInspectorCore;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
			
		spec project: 'GTSpotter' with: [
				spec
					className: #ConfigurationOfGTSpotter;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
		
		spec group: 'default' with: #('GTPlaygroundCore' 'GTInspectorCore' 'GTSpotter' ). ].
