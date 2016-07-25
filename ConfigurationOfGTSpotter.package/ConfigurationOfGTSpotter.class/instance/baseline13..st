baselines
baseline13: spec
	<version: '1.3-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec description: 'It depends on GT-EventRecorder.'.
		spec author: 'JurajKubelka'.
		spec timestamp: '2015-10-15'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		
		spec 
			package: 'GT-Spotter' with: [
				spec requires: #('GlamourCore' ). ];
			package: 'GT-Spotter-EventRecorder' with: [
				spec requires: #('GT-Spotter' 'GTEventRecorder' ). ];
			package: 'GT-SpotterExtensions-Core' with: [
				spec requires: #('GT-Spotter' 'GlamourCore' ) ];
			package: 'GT-Tests-Spotter' with: [
				spec requires: #('GT-Spotter' 'GT-SpotterExtensions-Core' ) ].
		
		spec 
			project: 'GlamourCore' with: [
				spec
					className: #ConfigurationOfGlamourCore;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/Glamour/main/' ].

		spec 
			project: 'GTEventRecorder' with: [
				spec
					className: #ConfigurationOfGTEventRecorder;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
		
		spec 
			group: 'Core' with: #('GT-Spotter' 'GT-SpotterExtensions-Core' );
			group: 'Tests' with: #('GT-Spotter' 'GT-Tests-Spotter');
			group: 'default' with: #('Core' 'Tests' 'GT-Spotter-EventRecorder') ]