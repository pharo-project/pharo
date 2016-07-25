versions
version25: spec
	<version: '2.5' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixes in the debugger, inspector and spotter.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/10/2016 23:53'.
		spec project: 'GlamourCore' with: '3.7'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-StefanReichhart.367';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.192';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.50'. ].
