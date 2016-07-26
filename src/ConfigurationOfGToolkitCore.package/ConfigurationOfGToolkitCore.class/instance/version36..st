versions
version36: spec
	<version: '3.6' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixes in the debugger, inspector and spotter.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/10/2016 23:53'.
		spec 
			project: 'GTPlaygroundCore' with: '2.6';
			project: 'GTInspectorCore' with: '2.5';
			project: 'GTSpotter' with: '1.8';
			project: 'GTDebugger' with: '1.3'. ].
