versions
version2111: spec
	<version: '2.1.11' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'bugfixes'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/15/2015 16:28'.
		spec project: 'GTInspectorCore' with: '2.0.22'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.115';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
