versions
version205: spec
	<version: '2.0.5' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.5'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/10/2014 13:38'.
		
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.275';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.87';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. 
		spec 
			project: 'GlamourCore' with: '3.0.5'. ].
