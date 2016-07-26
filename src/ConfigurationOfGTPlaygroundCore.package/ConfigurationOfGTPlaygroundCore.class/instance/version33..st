versions
version33: spec
	<version: '3.3' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.14'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/28/2016 12:11'.
		spec project: 'GTInspectorCore' with: '3.3'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AliakseiSyrel.122';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
