versions
version200: spec
	<version: '2.0.0' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.0'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/29/2014 17:01'.
		
		spec 
			package: #'GT-Inspector' with: 'GT-Inspector-TudorGirba.266';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.74';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. 
		spec 
			project: 'GlamourCore' with: '3.0.0'. ].
