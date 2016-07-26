versions
version2020: spec
	<version: '2.0.20' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'remove  hard dependency to theme icons (they will be remove, and also it was bad :) )'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '10/1/2015 14:30'.
		spec project: 'GlamourCore' with: '3.2.2'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.310';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-EstebanLorenzano.160';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.34'. ].
