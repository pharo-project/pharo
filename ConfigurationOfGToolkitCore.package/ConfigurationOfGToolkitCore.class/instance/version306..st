versions
version306: spec
	<version: '3.0.6' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.6'.
		spec author: 'AndreiChis'.
		spec timestamp: '9/1/2015 15:33'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.9'. 
		spec project: 'GTInspectorCore' with: '2.0.19'.
		spec project: 'GTSpotter' with: '1.2.10'. ]
	
