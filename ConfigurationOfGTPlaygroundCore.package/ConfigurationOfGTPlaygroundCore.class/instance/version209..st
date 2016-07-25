versions
version209: spec
	<version: '2.0.9' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.9'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/16/2015 19:12'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.59'.
		spec project: 'GTInspectorCore' with: '2.0.10'. ]
