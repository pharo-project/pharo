versions
version312: spec
	<version: '3.12' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.12'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/20/2016 00:11'.
		spec 
			project: 'GTPlaygroundCore' with: '3.1';
			project: 'GTInspectorCore' with: '3.1';
			project: 'GTSpotter' with: '2.1';
			project: 'GTDebugger' with: '2.1'. ].
