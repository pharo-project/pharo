versions
version311: spec
	<version: '3.11' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.11'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/18/2016 14:21'.
		spec 
			project: 'GTPlaygroundCore' with: '3.0';
			project: 'GTInspectorCore' with: '3.0';
			project: 'GTSpotter' with: '2.0';
			project: 'GTDebugger' with: '2.0'. ].
