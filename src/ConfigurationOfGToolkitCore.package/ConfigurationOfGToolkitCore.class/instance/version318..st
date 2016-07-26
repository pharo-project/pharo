versions
version318: spec
	<version: '3.18' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.18'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/8/2016 10:36'.
		spec 
			project: 'GTPlaygroundCore' with: '3.7';
			project: 'GTInspectorCore' with: '3.7';
			project: 'GTSpotter' with: '2.7';
			project: 'GTDebugger' with: '2.6'. ].
