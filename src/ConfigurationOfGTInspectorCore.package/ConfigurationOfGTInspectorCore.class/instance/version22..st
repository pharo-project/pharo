versions
version22: spec
	<version: '2.2' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Update GTools. 
 - bug fixes
 - improvements in the event recorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/19/2015 20:57'.
		spec project: 'GlamourCore' with: '3.4'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.350';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.185';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.48'. ].
