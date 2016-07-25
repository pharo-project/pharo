versions
version304: spec
	<version: '3.0.4' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.4'.
		spec author: 'AndreiChis'.
		spec timestamp: '7/10/2015 17:31'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.7'. 
		spec project: 'GTInspectorCore' with: '2.0.17'.
		spec project: 'GTSpotter' with: '1.2.8'. ]
	
