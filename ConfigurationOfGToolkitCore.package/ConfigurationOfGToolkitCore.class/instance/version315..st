versions
version315: spec
	<version: '3.15' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools Version 3.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 10:50'.
		spec 
			project: 'GTPlaygroundCore' with: '3.4';
			project: 'GTInspectorCore' with: '3.4';
			project: 'GTSpotter' with: '2.4';
			project: 'GTDebugger' with: '2.3'. ].
