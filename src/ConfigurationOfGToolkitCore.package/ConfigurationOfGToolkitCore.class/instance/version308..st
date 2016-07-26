versions
version308: spec
	<version: '3.0.8' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.7'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/15/2015 16:24'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.11'. 
		spec project: 'GTInspectorCore' with: '2.0.22'.
		spec project: 'GTSpotter' with: '1.2.15'. ]
	
