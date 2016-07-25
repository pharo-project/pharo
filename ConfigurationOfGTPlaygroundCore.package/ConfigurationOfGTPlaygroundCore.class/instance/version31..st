versions
version31: spec
	<version: '3.1' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.12'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/20/2016 00:11'.
		spec project: 'GTInspectorCore' with: '3.1'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-TudorGirba.119';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
