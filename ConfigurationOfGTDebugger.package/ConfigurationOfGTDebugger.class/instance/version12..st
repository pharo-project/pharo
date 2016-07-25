versions
version12: spec
	<version: '1.2' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'This version fixes problems in GTDebugger and updates  Rubric (issues: 17155, 17279, 17152)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/8/2016 19:27'.
		spec project: 'GTInspectorCore' with: '2.4'.
		spec 
			package: 'GT-Debugger' with: 'GT-Debugger-AndreiChis.268';
			package: 'GT-BytecodeDebugger' with: 'GT-BytecodeDebugger-AndreiChis.24';
			package: 'GT-SUnitDebugger' with: 'GT-SUnitDebugger-AndreiChis.21';
			package: 'GT-Tests-Debugger' with: 'GT-Tests-Debugger-AndreiChis.5'. ].
