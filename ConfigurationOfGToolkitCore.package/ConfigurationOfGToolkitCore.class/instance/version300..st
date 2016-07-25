versions
version300: spec
	<version: '3.0.0' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.0'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/08/2015 22:11'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.0'. 
		spec project: 'GTInspectorCore' with: '2.0.11'.
		spec project: 'GTSpotter' with: '1.2.0'. ]
	
