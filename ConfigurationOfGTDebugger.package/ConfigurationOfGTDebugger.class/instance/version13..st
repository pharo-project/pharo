versions
version13: spec
	<version: '1.3' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixes in the debugger, inspector and spotter.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/10/2016 23:53'.
		spec project: 'GTInspectorCore' with: '2.5'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.293';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-AndreiChis.28';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.24';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.5'. ].
