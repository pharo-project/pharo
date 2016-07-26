versions
version311: spec
	<version: '3.11' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTInspector version 3.11'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/26/2016 11:42'.
		spec project: 'GlamourCore' with: '4.13'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.418';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.213';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
