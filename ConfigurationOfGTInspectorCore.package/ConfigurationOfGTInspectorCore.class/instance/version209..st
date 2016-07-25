versions
version209: spec
	<version: '2.0.9' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.9'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/08/2015 21:41'.
		spec project: 'GlamourCore' with: '3.0.9'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.279';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.92';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. ].
