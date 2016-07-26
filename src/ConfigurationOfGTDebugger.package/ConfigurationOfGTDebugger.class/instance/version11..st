versions
version11: spec
	<version: '1.1' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Load GTDebugger together with the extensions for SUnit and Bytecode.
Bugfixes (17190, 17206, 17336, 17337, 17225)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/7/2016 10:41'.
		spec project: 'GTInspectorCore' with: '2.3'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.264';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-AndreiChis.23';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.21';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.4'. ].
