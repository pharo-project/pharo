versions
version26: spec
	<version: '2.6' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.18'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/8/2016 10:36'.
		spec project: 'GTInspectorCore' with: '3.7'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.350';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-TudorGirba.39';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.14'. ].
