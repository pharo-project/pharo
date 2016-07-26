versions
version301: spec
	<version: '3.0.1' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/11/2015 18:05'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.1'. 
		spec project: 'GTInspectorCore' with: '2.0.12'.
		spec project: 'GTSpotter' with: '1.2.1'. ]
	
