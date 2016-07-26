versions
version2019: spec
	<version: '2.0.19' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.19'.
		spec author: 'AndreiChis'.
		spec timestamp: '9/1/2015'.
		spec project: 'GlamourCore' with: '3.1.10'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.310';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TorstenBergmann.159';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.34'. ].
