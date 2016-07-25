baselines
baseline05: spec
	<version: '0.5-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		spec author: 'AndreiChis'.
		spec description: 'Baseline for integrating GTDebugger in Pharo 5'.
		
		spec package: 'GT-Debugger' with: [ spec requires: #( 'GTInspectorCore' )].
		spec package: 'GT-BytecodeDebugger' with: [ spec	requires: #( 'GT-Debugger') ].
		spec package: 'GT-SUnitDebugger' with: [ spec requires: #( 'GT-Debugger') ].
		spec package: 'GT-Tests-Debugger' with: [ spec requires: #( 'GT-Debugger' 'GT-BytecodeDebugger' 'GT-SUnitDebugger' )].
		
		spec group: 'default' with: #('GT-Debugger' 'GT-BytecodeDebugger' 'GT-SUnitDebugger' 'GT-Tests-Debugger').
		
		spec project: 'GTInspectorCore' with: [
				spec
					className: #ConfigurationOfGTInspectorCore;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ] ].
