baselines
baseline04: spec
	<version: '0.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		spec description: 'Load just DebuggerExtensions from NewDebugger'.
		spec package: 'GT-Debugger' with: [ spec requires: #( 'GTInspector' 'NewDebugger' )].
		spec package: 'GT-Tests-Debugger' with: [ spec requires: #( 'GT-Debugger' )].
		spec group: 'Tests' with: #('GT-Tests-Debugger').
		spec group: 'default' with: #('GT-Debugger' 'Tests').
		
		spec project: 'GTInspector' with: [
				spec
					className: #ConfigurationOfGTInspector;
					versionString: #'development';
					repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/' ].
				
		spec project: 'NewDebugger' with: [
				spec
					versionString: #'development';
					className: 'ConfigurationOfNewDebugger';
					loads: #('DebuggerExtensions');
					repository: 'http://www.smalltalkhub.com/mc/Pharo/Debugger/main' ] ].
