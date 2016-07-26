versions
version25: spec
	<version: '2.5' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.17'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/29/2016 23:44'.
		spec project: 'GTInspectorCore' with: '3.6'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.349';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-TudorGirba.39';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.14'. ].
