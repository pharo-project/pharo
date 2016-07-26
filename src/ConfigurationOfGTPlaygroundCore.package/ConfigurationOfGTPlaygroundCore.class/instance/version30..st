versions
version30: spec
	<version: '3.0' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.11'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/18/2016 14:21'.
		spec project: 'GTInspectorCore' with: '3.0'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-TudorGirba.119';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
