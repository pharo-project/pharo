versions
version202: spec
	<version: '2.0.2' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.2'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/14/2014 11:48'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-TudorGirba.49'.
		spec project: 'GTInspectorCore' with: '2.0.2'. ]
