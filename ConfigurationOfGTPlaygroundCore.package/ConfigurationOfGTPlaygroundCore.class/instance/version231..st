versions
version231: spec
	<version: '2.3.1' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Changes in the use of #garbageCollect'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/25/2015 18:00'.
		spec project: 'GTInspectorCore' with: '2.2.1'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.117';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
