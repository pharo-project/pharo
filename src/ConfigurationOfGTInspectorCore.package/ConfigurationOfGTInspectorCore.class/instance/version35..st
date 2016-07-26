versions
version35: spec
	<version: '3.5' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/23/2016 14:21'.
		spec project: 'GlamourCore' with: '4.8'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AlexandreBergel.389';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.205';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
