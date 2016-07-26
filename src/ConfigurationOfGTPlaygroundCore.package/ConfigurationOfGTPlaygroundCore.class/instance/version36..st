versions
version36: spec
	<version: '3.6' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.17'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/29/2016 23:44'.
		spec project: 'GTInspectorCore' with: '3.6'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AliakseiSyrel.123';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
