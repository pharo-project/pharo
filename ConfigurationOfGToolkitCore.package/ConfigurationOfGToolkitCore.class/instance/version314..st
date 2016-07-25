versions
version314: spec
	<version: '3.14' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.14'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/28/2016 12:11'.
		spec 
			project: 'GTPlaygroundCore' with: '3.3';
			project: 'GTInspectorCore' with: '3.3';
			project: 'GTSpotter' with: '2.3';
			project: 'GTDebugger' with: '2.2'. ].
