versions
version2011: spec
	<version: '2.0.11' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.11'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/06/2015 22:04'.
		spec project: 'GlamourCore' with: '3.1.0'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.305';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.144';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
