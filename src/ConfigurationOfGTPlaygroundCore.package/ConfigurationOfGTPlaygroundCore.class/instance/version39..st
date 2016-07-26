versions
version39: spec
	<version: '3.9' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.20 (case 18022)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/18/2016 22:57'.
		spec project: 'GTInspectorCore' with: '3.9'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-DenisKudryashov.124';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
