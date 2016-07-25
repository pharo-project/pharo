versions
version302: spec
	<version: '3.0.2' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.2'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/29/2015 11:13'.
		
		spec project: 'GTPlaygroundCore' with: '2.1.2'. 
		spec project: 'GTInspectorCore' with: '2.0.13'.
		spec project: 'GTSpotter' with: '1.2.2'. ]
	
