versions
version27: spec
	<version: '2.7' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in Spotter and Glamour'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 16:23'.
		spec project: 'GTInspectorCore' with: '2.6'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.118';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
