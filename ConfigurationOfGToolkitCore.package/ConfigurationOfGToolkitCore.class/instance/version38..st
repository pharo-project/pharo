versions
version38: spec
	<version: '3.8' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in inspector and debugger'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 18:15'.
		spec 
			project: 'GTPlaygroundCore' with: '2.8';
			project: 'GTInspectorCore' with: '2.7';
			project: 'GTSpotter' with: '1.10';
			project: 'GTDebugger' with: '1.4'. ].
