versions
version29: spec
	<version: '2.9' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '02/08/2016 15:53'.
		spec project: 'GlamourCore' with: '4.0'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.382';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.200';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
