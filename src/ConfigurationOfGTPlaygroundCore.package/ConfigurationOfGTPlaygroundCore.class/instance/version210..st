versions
version210: spec
	<version: '2.10' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/23/2016 15:53'.
		spec project: 'GTInspectorCore' with: '2.9'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-TudorGirba.119';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
