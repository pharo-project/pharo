versions
version28: spec
	<version: '2.8' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in inspector and debugger'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 18:14'.
		spec project: 'GTInspectorCore' with: '2.7'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.118';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
