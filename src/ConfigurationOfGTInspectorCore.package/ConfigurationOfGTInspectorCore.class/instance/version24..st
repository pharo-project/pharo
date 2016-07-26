versions
version24: spec
	<version: '2.4' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'This version fixes problems in GTDebugger and updates  Rubric (issues: 17155, 17279, 17152)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/8/2016 19:27'.
		spec project: 'GlamourCore' with: '3.6'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.360';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.188';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.50'. ].
