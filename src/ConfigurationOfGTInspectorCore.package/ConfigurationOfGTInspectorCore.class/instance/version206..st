versions
version206: spec
	<version: '2.0.6' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.6'.
		spec author: 'TudorGirba'.
		spec timestamp: '12/23/2014 14:07'.
		spec project: 'GlamourCore' with: '3.0.6'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.276';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.88';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. ].
