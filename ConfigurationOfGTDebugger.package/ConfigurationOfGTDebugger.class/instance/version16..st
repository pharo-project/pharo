versions
version16: spec
	<version: '1.6' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '02/08/2016 15:54'.
		spec project: 'GTInspectorCore' with: '2.9'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.312';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-TudorGirba.31';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.32';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.11'. ].
