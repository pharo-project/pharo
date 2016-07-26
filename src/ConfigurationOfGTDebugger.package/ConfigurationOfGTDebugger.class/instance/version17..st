versions
version17: spec
	<version: '1.7' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bug fix 17602'.
		spec author: 'AndreiChis'.
		spec timestamp: '02/08/2016 15:54'.
		spec project: 'GTInspectorCore' with: '2.9'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-DenisKudryashov.313';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.32';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-TheIntegrator.13'. ].
