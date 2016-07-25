versions
version321: spec
	<version: '3.21' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.21 (case 18112)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/27/2016 19:10'.
		spec 
			project: 'GTPlaygroundCore' with: '3.9';
			project: 'GTInspectorCore' with: '3.12';
			project: 'GTSpotter' with: '2.10';
			project: 'GTDebugger' with: '2.8'. ].
