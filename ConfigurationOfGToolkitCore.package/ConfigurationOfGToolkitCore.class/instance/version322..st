versions
version322: spec
	<version: '3.22' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.22 (case 17948)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/28/2016 16:25'.
		spec 
			project: 'GTPlaygroundCore' with: '3.10';
			project: 'GTInspectorCore' with: '3.14';
			project: 'GTSpotter' with: '2.11';
			project: 'GTDebugger' with: '2.9'. ].
