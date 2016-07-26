versions
version23: spec
	<version: '2.3' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools Version 3.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 10:50'.
		spec project: 'GTInspectorCore' with: '3.4'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-MarcusDenker.329';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-TudorGirba.39';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-TheIntegrator.13'. ].
