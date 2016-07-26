versions
version203: spec
	<version: '2.0.3' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.3'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/07/2014 13:48'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.52'.
		spec project: 'GTInspectorCore' with: '2.0.3'. ]
