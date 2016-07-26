versions
version316: spec
	<version: '3.16' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/23/2016 14:22'.
		spec 
			project: 'GTPlaygroundCore' with: '3.5';
			project: 'GTInspectorCore' with: '3.5';
			project: 'GTSpotter' with: '2.5';
			project: 'GTDebugger' with: '2.4'. ].
