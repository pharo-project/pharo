versions
version38: spec
	<version: '3.8' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.19'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/14/2016 13:57'.
		spec project: 'GTInspectorCore' with: '3.8'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-DenisKudryashov.124';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
