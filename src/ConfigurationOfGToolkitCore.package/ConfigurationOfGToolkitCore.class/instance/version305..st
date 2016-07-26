versions
version305: spec
	<version: '3.0.5' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.5'.
		spec author: 'AndreiChis'.
		spec timestamp: '8/12/2015 15:33'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.8'. 
		spec project: 'GTInspectorCore' with: '2.0.18'.
		spec project: 'GTSpotter' with: '1.2.9'. ]
	
