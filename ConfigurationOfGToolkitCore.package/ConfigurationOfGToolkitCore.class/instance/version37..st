versions
version37: spec
	<version: '3.7' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in Spotter and Glamour'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 16:23'.
		spec 
			project: 'GTPlaygroundCore' with: '2.7';
			project: 'GTInspectorCore' with: '2.6';
			project: 'GTSpotter' with: '1.9';
			project: 'GTDebugger' with: '1.3'. ].
