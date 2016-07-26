versions
version24: spec
	<version: '2.4' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/23/2016 14:21'.
		spec project: 'GTInspectorCore' with: '3.5'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-MarcusDenker.329';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-TudorGirba.39';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-TheIntegrator.13'. ].
