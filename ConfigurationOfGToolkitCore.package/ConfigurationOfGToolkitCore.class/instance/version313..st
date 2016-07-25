versions
version313: spec
	<version: '3.13' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.13'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/23/2016 17:57'.
		spec 
			project: 'GTPlaygroundCore' with: '3.2';
			project: 'GTInspectorCore' with: '3.2';
			project: 'GTSpotter' with: '2.2';
			project: 'GTDebugger' with: '2.1'. ].
