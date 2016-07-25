versions
version311: spec
	<version: '3.11' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit Version 3.23'.
		spec author: 'AndreiChis'.
		spec timestamp: '6/27/2016 12:09'.
		spec project: 'GTInspectorCore' with: '3.15'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.127';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
