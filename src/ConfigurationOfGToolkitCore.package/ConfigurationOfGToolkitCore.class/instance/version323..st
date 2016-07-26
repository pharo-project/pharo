versions
version323: spec
	<version: '3.23' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit Version 3.23'.
		spec author: 'AndreiChis'.
		spec timestamp: '6/27/2016 12:09'.
		spec 
			project: 'GTPlaygroundCore' with: '3.11';
			project: 'GTInspectorCore' with: '3.15';
			project: 'GTSpotter' with: '2.12';
			project: 'GTDebugger' with: '2.10'. ].
