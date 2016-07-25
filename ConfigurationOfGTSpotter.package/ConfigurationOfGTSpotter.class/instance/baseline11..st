baselines
baseline11: spec
	<version: '1.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec description: 'GT-Spotter-EventRecorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '2015-02-28'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		
		spec 
			package: 'GT-Spotter' with: [
				spec requires: #('GlamourCore' ). ];
			package: 'GT-Spotter-EventRecorder' with: [
				spec requires: #('GT-Spotter' ). ];
			package: 'GT-SpotterExtensions-Core' with: [
				spec requires: #('GT-Spotter' 'GlamourCore' ). ].
		
		spec 
			project: 'GlamourCore' with: [
				spec
					className: #ConfigurationOfGlamourCore;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/Glamour/main/' ].
		
		spec 
			group: 'Core' with: #('GT-Spotter' 'GT-SpotterExtensions-Core' );
			group: 'Tests' with: #('GT-Spotter' );
			group: 'default' with: #('Core' 'GT-Spotter-EventRecorder') ]