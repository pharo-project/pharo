versions
version32: spec
	<version: '3.2' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.13'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/23/2016 17:57'.
		spec project: 'GTInspectorCore' with: '3.2'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-TudorGirba.119';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
