versions
version200: spec
	<version: '2.0.0' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.0'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/29/2014 16:54'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.48'.
		spec project: 'GTInspectorCore' with: '2.0.0'. ]
