versions
version208: spec
	<version: '2.0.8' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.8'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/07/2015 17:28'.
		spec project: 'GlamourCore' with: '3.0.8'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.278';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.91';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. ].
