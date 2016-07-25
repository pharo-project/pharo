versions
version207: spec
	<version: '2.0.7' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.7'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/07/2015 17:25'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-TudorGirba.54'.
		spec project: 'GTInspectorCore' with: '2.0.8'. ]
