baselines
baseline20: spec
	<version: '2.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec description: 'GTInspector Core'.
		spec author: 'AndreiChis'.
		spec timestamp: '2014-10-29'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		
		spec 
			package: 'GT-Inspector' with: [
				spec requires: #('GlamourCore' ). ];
			package: 'GT-InspectorExtensions-Core' with: [
				spec requires: #('GT-Inspector' 'GlamourCore' ). ];
			package: 'GT-Tests-Inspector' with: [
				spec requires: #('GT-Inspector' 'GT-InspectorExtensions-Core' ). ].
		
		spec 
			project: 'GlamourCore' with: [
				spec
					className: #ConfigurationOfGlamourCore;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/Glamour/main/' ].
		
		spec 
			group: 'Core' with: #('GT-Inspector' 'GT-InspectorExtensions-Core' );
			group: 'Tests' with: #('GT-Tests-Inspector' );
			group: 'default' with: #('GT-Inspector' 'GT-InspectorExtensions-Core' 'GT-Tests-Inspector' ) ]
