versions
version201: spec
	<version: '2.0.1' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/13/2014 13:54'.
		
		spec 
			package: #'GT-Inspector' with: 'GT-Inspector-BernardoContreras.267';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.76';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. 
		spec 
			project: 'GlamourCore' with: '3.0.1'. ].
