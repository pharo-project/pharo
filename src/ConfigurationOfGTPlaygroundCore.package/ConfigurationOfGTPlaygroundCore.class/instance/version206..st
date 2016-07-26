versions
version206: spec
	<version: '2.0.6' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.6'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/24/2014 10:50'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.53'.
		spec project: 'GTInspectorCore' with: '2.0.6'. ]
