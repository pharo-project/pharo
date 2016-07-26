versions
version24: spec
	<version: '2.4' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Load GTDebugger together with the extensions for SUnit and Bytecode.
Bugfixes (17190, 17206, 17336, 17337, 17225)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/7/2016 10:41'.
		spec project: 'GTInspectorCore' with: '2.3'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.117';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
