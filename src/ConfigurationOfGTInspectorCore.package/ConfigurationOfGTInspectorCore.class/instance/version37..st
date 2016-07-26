versions
version37: spec
	<version: '3.7' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.18'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/8/2016 10:36'.
		spec project: 'GlamourCore' with: '4.10'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-StefanReichhart.397';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.213';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
