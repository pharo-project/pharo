versions
version34: spec
	<version: '3.4' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools Version 3.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 10:49'.
		spec project: 'GTInspectorCore' with: '3.4'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AliakseiSyrel.122';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
