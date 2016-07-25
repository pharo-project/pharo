versions
version37: spec
	<version: '3.7' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.18'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/8/2016 10:36'.
		spec project: 'GTInspectorCore' with: '3.7'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AliakseiSyrel.123';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
