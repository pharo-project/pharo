versions
version35: spec
	<version: '3.5' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'This version fixes problems in GTDebugger and updates  Rubric (issues: 17155, 17279, 17152)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/8/2016 19:27'.
		spec 
			project: 'GTPlaygroundCore' with: '2.5';
			project: 'GTInspectorCore' with: '2.4';
			project: 'GTSpotter' with: '1.7';
			project: 'GTDebugger' with: '1.2'. ].
