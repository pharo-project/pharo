versions
version14: spec
	<version: '1.4' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in inspector and debugger'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 18:14'.
		spec project: 'GTInspectorCore' with: '2.7'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.297';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-AndreiChis.28';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.24';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.9'. ].
