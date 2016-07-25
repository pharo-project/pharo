versions
version317: spec
	<version: '3.17' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.17'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/29/2016 23:44'.
		spec 
			project: 'GTPlaygroundCore' with: '3.6';
			project: 'GTInspectorCore' with: '3.6';
			project: 'GTSpotter' with: '2.6';
			project: 'GTDebugger' with: '2.5'. ].
