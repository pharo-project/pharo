versions
version208: spec
	<version: '2.0.8' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.8'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/08/2015 21:43'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.56'.
		spec project: 'GTInspectorCore' with: '2.0.9'. ]
