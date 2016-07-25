versions
version201: spec
	<version: '2.0.1' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/13/2014 13:54'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-TudorGirba.49'.
		spec project: 'GTInspectorCore' with: '2.0.1'. ]
