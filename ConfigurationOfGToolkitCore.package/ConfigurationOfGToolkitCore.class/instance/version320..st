versions
version320: spec
	<version: '3.20' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.20 (case 18022)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/18/2016 22:57'.
		spec 
			project: 'GTPlaygroundCore' with: '3.9';
			project: 'GTInspectorCore' with: '3.9';
			project: 'GTSpotter' with: '2.9';
			project: 'GTDebugger' with: '2.8'. ].
