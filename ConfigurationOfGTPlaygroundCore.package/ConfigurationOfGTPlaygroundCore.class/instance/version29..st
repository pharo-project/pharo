versions
version29: spec
	<version: '2.9' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes and improvements.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/18/2016 13:24'.
		spec project: 'GTInspectorCore' with: '2.8'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.118';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
