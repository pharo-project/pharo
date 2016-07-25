versions
version34: spec
	<version: '3.4' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools Version 3.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 10:49'.
		spec project: 'GlamourCore' with: '4.6'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AlexandreBergel.389';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.204';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
