baselines
baseline01: spec
	<version: '0.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		spec package: 'GT-Debugger' with: [ spec requires: #( 'GTInspector' 'NewDebugger' )].
		spec group: 'default' with: #('GT-Debugger').
		
		spec project: 'GTInspector' with: [
				spec
					className: 'ConfigurationOfGToolkit';
					file: 'ConfigurationOfGToolkit';
					version: #development;
					loads: #('GT-Inspector');
					repository: 'http://www.smalltalkhub.com/mc/Moose/GToolkit/main' ].
				
		spec project: 'NewDebugger' with: [
				spec
					versionString: #'development';
					className: 'ConfigurationOfNewDebugger';
					loads: #( 'Core' 'DebuggerExtensions');
					repository: 'http://www.smalltalkhub.com/mc/Pharo/Debugger/main' ] ].
