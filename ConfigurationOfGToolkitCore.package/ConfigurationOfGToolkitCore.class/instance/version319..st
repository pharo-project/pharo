versions
version319: spec
	<version: '3.19' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.19'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/14/2016 13:57'.
		spec 
			project: 'GTPlaygroundCore' with: '3.8';
			project: 'GTInspectorCore' with: '3.8';
			project: 'GTSpotter' with: '2.8';
			project: 'GTDebugger' with: '2.7'. ].
