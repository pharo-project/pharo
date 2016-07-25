versions
version35: spec
	<version: '3.5' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/23/2016 14:21'.
		spec project: 'GTInspectorCore' with: '3.5'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AliakseiSyrel.122';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
