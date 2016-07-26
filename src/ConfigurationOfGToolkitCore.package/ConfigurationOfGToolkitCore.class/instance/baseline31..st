baselines
baseline31: spec
	<version: '3.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec author: 'AndreiChis'.
		spec description: 'Load GTDebugger'.
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
				
		spec project: 'GTDebugger' with: [
				spec
					className: #ConfigurationOfGTDebugger;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
		
		spec group: 'default' with: #(
			'GTPlaygroundCore' 
			'GTInspectorCore' 
			'GTSpotter' 
			'GTDebugger' ). ].
