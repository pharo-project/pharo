versions
version2110: spec
	<version: '2.1.10' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'remove  hard dependency to theme icons (they will be remove, and also it was bad :) )'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '10/1/2015 14:30'.
		spec project: 'GTInspectorCore' with: '2.0.20'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-EstebanLorenzano.113';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
