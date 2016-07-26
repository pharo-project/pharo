versions
version15: spec
	<version: '1.5' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes and improvements.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/18/2016 13:24'.
		spec project: 'GTInspectorCore' with: '2.8'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.307';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-AndreiChis.28';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.24';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-TudorGirba.8'. ].
