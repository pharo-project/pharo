versions
version33: spec
	<version: '3.3' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Load GTDebugger together with the extensions for SUnit and Bytecode.
Bugfixes (17190, 17206, 17336, 17337, 17225)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/7/2016 10:41'.
		spec 
			project: 'GTPlaygroundCore' with: '2.4';
			project: 'GTInspectorCore' with: '2.3';
			project: 'GTSpotter' with: '1.5';
			project: 'GTDebugger' with: '1.1'. ].
