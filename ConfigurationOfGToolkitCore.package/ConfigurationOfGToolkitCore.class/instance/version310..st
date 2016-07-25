versions
version310: spec
	<version: '3.10' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '02/08/2016 15:54'.
		spec 
			project: 'GTPlaygroundCore' with: '2.10';
			project: 'GTInspectorCore' with: '2.9';
			project: 'GTSpotter' with: '1.12';
			project: 'GTDebugger' with: '1.6'. ].
