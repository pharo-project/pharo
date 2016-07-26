versions
version204: spec
	<version: '2.0.4' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.4'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/09/2014 12:08'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.52'.
		spec project: 'GTInspectorCore' with: '2.0.4'. ]
