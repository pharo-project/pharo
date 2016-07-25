versions
version39: spec
	<version: '3.9' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes and improvements.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/18/2016 13:24'.
		spec 
			project: 'GTPlaygroundCore' with: '2.9';
			project: 'GTInspectorCore' with: '2.8';
			project: 'GTSpotter' with: '1.11';
			project: 'GTDebugger' with: '1.5'. ].
