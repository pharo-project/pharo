versions
version2014: spec
	<version: '2.0.14' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.14'.
		spec author: 'AndreiChis'.
		spec timestamp: '05/05/2015'.
		spec project: 'GlamourCore' with: '3.1.3'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.305';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.147';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
