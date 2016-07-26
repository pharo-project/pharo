versions
version29: spec
	<version: '2.9' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.22 (case 17948)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/28/2016 16:25'.
		spec project: 'GTInspectorCore' with: '3.14'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.350';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-TudorGirba.39';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.14'. ].
