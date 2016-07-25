versions
version2015: spec
	<version: '2.0.15' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '5/15/2015'.
		spec project: 'GlamourCore' with: '3.1.4'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.307';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.147';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
