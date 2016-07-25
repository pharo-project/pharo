versions
version310: spec
	<version: '3.10' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.22 (case 17948)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/28/2016 16:25'.
		spec project: 'GTInspectorCore' with: '3.14'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-DenisKudryashov.124';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
